using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Globalization;
using Python.Runtime;

namespace PythonNetTest
{

    public partial class Form1 : Form
    {
        PyObject scenario,result;
        System.Threading.Thread thread;
        IntPtr threadstate;

        public Form1()
        {
            InitializeComponent();

            // Make sure that the GUI libraries can be found [note: we need several individual .py library files,
            // no longer a single module that takes control. Thus we need a directory rather than a path to a file]
            System.Environment.SetEnvironmentVariable("PYTHONPATH", @"Z:\\Documents\\Programmeren\\GOTM-CVS\\gotm\\gui.py;");

            // Initialize the Python engine.
            PythonEngine.Initialize();

            // Must be called once, presumably to set the default thread state to NULL
            // (rather than the thread state of the main thread, which would prevent a worker thread
            // from taking control of the Python interpreter)
            // After BeginAllowThreads, AcquireLock/ReleaseLock combinations can be used in any thread.
            threadstate = PythonEngine.BeginAllowThreads();

            // Preload a scenario because I am getting tired of browsing to an exising one via dialogs.
            IntPtr pyLock = PythonEngine.AcquireLock();
            PyObject corescenario = PythonEngine.ImportModule("core.scenario");
            this.scenario = corescenario.GetAttr("Scenario").InvokeMethod("fromSchemaName",new PyString("gotmgui-0.5.0"));
            this.scenario.InvokeMethod("loadAll", new PyString("\\\\pc-jorn\\Users\\Jorn\\Documents\\GOTM test cases\\gotmscenario 11-9-2007\\nns_annual.gotmscenario"));
            corescenario.Dispose();
            PythonEngine.ReleaseLock(pyLock);
        }

        private void button1_Click(object sender, EventArgs e)
        {
            IntPtr pyLock = PythonEngine.AcquireLock();
            PyObject scenariobuilder = PythonEngine.ImportModule("scenariobuilder");
            scenariobuilder.InvokeMethod("editScenario", scenario);

            // Demonstrate reading out scenario properties:
            //PyObject sn = scenario["station/name"].InvokeMethod("getValue");
            //PyObject o = scenario["title"].InvokeMethod("getValue");

            PythonEngine.ReleaseLock(pyLock);
        }

        private void button2_Click(object sender, EventArgs e)
        {
            this.thread = new System.Threading.Thread(this.simulate);
            this.thread.Start();
        }

        private void simulate()
        {
            IntPtr pyLock = PythonEngine.AcquireLock();

            // Proof of concept: retrieve time range using in the GOTM scenario
            //PyObject pystart = scenario["time/start"].InvokeMethod("getValue").InvokeMethod("isoformat");
            //PyObject pystop = scenario["time/stop"].InvokeMethod("getValue").InvokeMethod("isoformat");
            //DateTime start = DateTime.Parse((string)pystart.AsManagedObject(typeof(string)), CultureInfo.InvariantCulture, DateTimeStyles.AdjustToUniversal | DateTimeStyles.AssumeUniversal);
            //DateTime stop = DateTime.Parse((string)pystop.AsManagedObject(typeof(string)), CultureInfo.InvariantCulture, DateTimeStyles.AdjustToUniversal | DateTimeStyles.AssumeUniversal);
            //TimeSpan timesp = stop - start;

            // Determine the number of GOTM time steps in a month.
            PyObject pydt = scenario["timeintegration/dt"].InvokeMethod("getValue").InvokeMethod("getAsSeconds");
            float dt = (float)pydt.AsManagedObject(typeof(float));
            int slabsize = (int)Math.Round(30 * 24 * 3600 / dt);

            // Create the simulator in Python.
            PyObject simulator = PythonEngine.ImportModule("core.simulator");
            simulator = simulator.InvokeMethod("Simulator", scenario);

            // At this point GOTM has initialized and everything is known, including the names, units etc.
            // of the biological
            //String xml = scenario.InvokeMethod("describe");
            PyTuple bioinfo = new PyTuple(simulator.InvokeMethod("getBioVariableInfo"));
            string[] names = (string[])bioinfo[1].AsManagedObject(typeof(string[]));
            string[] units = (string[])bioinfo[2].AsManagedObject(typeof(string[]));

            bool hasmore = true;
            while (hasmore)
            {
                // Run a new slab.
                hasmore = simulator.InvokeMethod("runSlab",new PyInt(slabsize)).IsTrue();

                // Send fraction complete to progress bar.
                float progress = (float)simulator.InvokeMethod("getProgress").AsManagedObject(typeof(float));
                this.progressBar1.BeginInvoke((MethodInvoker)(delegate {this.progressBar1.Value = (int)Math.Round(progress * 100);}));

                // Get a string describing the current depth-integrated bio values.
                PyObject biovals = simulator.InvokeMethod("getBioValues");
                double[] vals = (double[])biovals.AsManagedObject(typeof(double[]));
                string biotext = "";
                for (int i = 0; i < names.Length; i++) biotext += names[i] + " = " + vals[i].ToString() + " " + units[i] + "\r\n";
                this.progressBar1.BeginInvoke((MethodInvoker)(delegate {this.textBox1.Text = biotext; }));
            }

            // Clean up after the run and obtain the result.
            result = simulator.InvokeMethod("finalize");

            PythonEngine.ReleaseLock(pyLock);
        }

        private void button3_Click(object sender, EventArgs e)
        {
            IntPtr pyLock = PythonEngine.AcquireLock();
            PyObject simulator = PythonEngine.ImportModule("visualizer");
            simulator.InvokeMethod("visualizeResult", result);
            PythonEngine.ReleaseLock(pyLock);
        }

        private void button4_Click(object sender, EventArgs e)
        {
            IntPtr pyLock = PythonEngine.AcquireLock();
            PyObject scenariobuilder = PythonEngine.ImportModule("scenariobuilder");
            PyObject newscenario = scenariobuilder.InvokeMethod("loadScenario");

            // If the user cancelled loading a new scenario, just keep the old one.
            if (newscenario.IsTrue()) scenario = newscenario;

            PythonEngine.ReleaseLock(pyLock);
        }

    }
}