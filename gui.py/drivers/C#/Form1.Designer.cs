namespace PythonNetTest
{
    partial class Form1
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.buttonEdit = new System.Windows.Forms.Button();
            this.buttonSimulate = new System.Windows.Forms.Button();
            this.progressBar1 = new System.Windows.Forms.ProgressBar();
            this.buttonShowResults = new System.Windows.Forms.Button();
            this.buttonOpen = new System.Windows.Forms.Button();
            this.textBox1 = new System.Windows.Forms.TextBox();
            this.SuspendLayout();
            // 
            // buttonEdit
            // 
            this.buttonEdit.Enabled = false;
            this.buttonEdit.Location = new System.Drawing.Point(12, 44);
            this.buttonEdit.Name = "buttonEdit";
            this.buttonEdit.Size = new System.Drawing.Size(111, 23);
            this.buttonEdit.TabIndex = 1;
            this.buttonEdit.Text = "Edit scenario";
            this.buttonEdit.UseVisualStyleBackColor = true;
            this.buttonEdit.Click += new System.EventHandler(this.button1_Click);
            // 
            // buttonSimulate
            // 
            this.buttonSimulate.Enabled = false;
            this.buttonSimulate.Location = new System.Drawing.Point(12, 73);
            this.buttonSimulate.Name = "buttonSimulate";
            this.buttonSimulate.Size = new System.Drawing.Size(111, 23);
            this.buttonSimulate.TabIndex = 2;
            this.buttonSimulate.Text = "Simulate scenario";
            this.buttonSimulate.UseVisualStyleBackColor = true;
            this.buttonSimulate.Click += new System.EventHandler(this.button2_Click);
            // 
            // progressBar1
            // 
            this.progressBar1.Location = new System.Drawing.Point(12, 102);
            this.progressBar1.Name = "progressBar1";
            this.progressBar1.Size = new System.Drawing.Size(111, 23);
            this.progressBar1.TabIndex = 3;
            // 
            // buttonShowResults
            // 
            this.buttonShowResults.Enabled = false;
            this.buttonShowResults.Location = new System.Drawing.Point(12, 131);
            this.buttonShowResults.Name = "buttonShowResults";
            this.buttonShowResults.Size = new System.Drawing.Size(111, 23);
            this.buttonShowResults.TabIndex = 4;
            this.buttonShowResults.Text = "Show results";
            this.buttonShowResults.UseVisualStyleBackColor = true;
            this.buttonShowResults.Click += new System.EventHandler(this.button3_Click);
            // 
            // buttonOpen
            // 
            this.buttonOpen.Location = new System.Drawing.Point(12, 15);
            this.buttonOpen.Name = "buttonOpen";
            this.buttonOpen.Size = new System.Drawing.Size(111, 23);
            this.buttonOpen.TabIndex = 0;
            this.buttonOpen.Text = "Open new scenario";
            this.buttonOpen.UseVisualStyleBackColor = true;
            this.buttonOpen.Click += new System.EventHandler(this.button4_Click);
            // 
            // textBox1
            // 
            this.textBox1.Location = new System.Drawing.Point(12, 160);
            this.textBox1.Multiline = true;
            this.textBox1.Name = "textBox1";
            this.textBox1.ReadOnly = true;
            this.textBox1.Size = new System.Drawing.Size(260, 114);
            this.textBox1.TabIndex = 5;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(284, 286);
            this.Controls.Add(this.textBox1);
            this.Controls.Add(this.buttonOpen);
            this.Controls.Add(this.buttonShowResults);
            this.Controls.Add(this.progressBar1);
            this.Controls.Add(this.buttonSimulate);
            this.Controls.Add(this.buttonEdit);
            this.Name = "Form1";
            this.Text = "Form1";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button buttonEdit;
        private System.Windows.Forms.Button buttonSimulate;
        private System.Windows.Forms.ProgressBar progressBar1;
        private System.Windows.Forms.Button buttonShowResults;
        private System.Windows.Forms.Button buttonOpen;
        private System.Windows.Forms.TextBox textBox1;
    }
}

