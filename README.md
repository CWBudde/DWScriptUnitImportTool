# DWScript Unit Import Tool
When working with DWScript as a scripting language for automations it's often necessary to import an API to make it scriptable. In the past this had to be done manually class by class, definition by definition. This tool does it directly to a point where the unit structure can be used as a skeleton for further implementations.

In order to use it, just open a file you want to make scriptable. It will show in the first tab as source code. After you have processed the code, it will appear on the next tabs as dfm data and a skeleton unit data. These files can be saved and the unit can be added to any project.

However, the code as such will not yet run out of the box. You need to link the structure to the code first. This can be done by writing the OnEval event handler.
