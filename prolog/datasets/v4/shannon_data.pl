% 1. ENTITIES & INTERVALS
% ------------------------------------------------------------------------------
% Core Theoretical Frameworks
entity(shannon_theory, class).
entity(logarithmic_measure, structural).
entity(binary_digit, structural).
entity(source_entropy, structural).
entity(channel_capacity, structural).
entity(equivocation, structural).

% Historical Predecessors (Alternatives)
entity(nyquist_hartley_papers, class).
entity(hartley_log_measure, structural).
entity(linear_possibility_measure, class).

% Systems / Stakeholders
entity(communication_engineering, organizational).
entity(telegraphy_system, individual).
entity(computing_machine, individual).

% Scaffolds (Theoretical tools used for transition)
entity(markoff_process_model, scaffold).
entity(discrete_approximation_limit, scaffold).

% Define Time Interval (1924 to 1948 and beyond)
% T=0: Nyquist (1924)
% T=4: Hartley (1928)
% T=24: Shannon Publication (1948)
% T=100: Long-term Stability
interval(information_theory_genesis, 0, 100).

% 2. CONSTRAINT CLAIMS & METRICS
% ------------------------------------------------------------------------------
% Shannon claims the logarithmic measure is a "Mountain" (Natural Law/Mathematical Necessity)
constraint_claim(logarithmic_measure, mountain).
constraint_metric(logarithmic_measure, extractiveness, 0.01).
constraint_metric(logarithmic_measure, suppression_requirement, 0.02).
constraint_metric(logarithmic_measure, resistance, 0.01).

% Channel Capacity (Noisy) is a fundamental limit (Mountain)
constraint_claim(channel_capacity, mountain).
constraint_metric(channel_capacity, extractiveness, 0.01).
constraint_metric(channel_capacity, suppression_requirement, 0.05).
constraint_metric(channel_capacity, resistance, 0.01).

% 3. TEMPORAL MEASUREMENTS (Triple Metrics Required)
% ------------------------------------------------------------------------------

% Evolution of the Logarithmic Measure of Information
% T=4: Hartley's Introduction
measurement(m1, logarithmic_measure, extractiveness, 4, 0.10).
measurement(m2, logarithmic_measure, suppression_requirement, 4, 0.05).
measurement(m3, logarithmic_measure, resistance, 4, 0.15).

% T=24: Shannon's Formalization (Standardization)
measurement(m4, logarithmic_measure, extractiveness, 24, 0.02).
measurement(m5, logarithmic_measure, suppression_requirement, 24, 0.02).
measurement(m6, logarithmic_measure, resistance, 24, 0.05).

% T=100: Universal Adoption (Mathematical Axiom Status)
measurement(m7, logarithmic_measure, extractiveness, 100, 0.00).
measurement(m8, logarithmic_measure, suppression_requirement, 100, 0.00).
measurement(m9, logarithmic_measure, resistance, 100, 0.00).

% Evolution of Source Entropy (H) as a constraint on compression
measurement(m10, source_entropy, extractiveness, 24, 0.05).
measurement(m11, source_entropy, suppression_requirement, 24, 0.05).
measurement(m12, source_entropy, resistance, 24, 0.10).

measurement(m13, source_entropy, extractiveness, 100, 0.01).
measurement(m14, source_entropy, suppression_requirement, 100, 0.01).
measurement(m15, source_entropy, resistance, 100, 0.01).

% 4. VIABLE ALTERNATIVES (Signature Detection)
% ------------------------------------------------------------------------------
% The measure of information had alternatives (Coordination Scaffold Signature)
intent_viable_alternative(information_theory_genesis, hartley_log_measure, 'Linear number of possibilities as measure').
intent_alternative_rejected(information_theory_genesis, hartley_log_measure, 'Logarithmic is practically more useful and mathematically suitable').

% Coding methods (Theorem 9 and 11) have alternatives (Fano Coding)
intent_viable_alternative(information_theory_genesis, shannon_fano_coding, 'R.M. Fano independent coding method').
intent_alternative_rejected(information_theory_genesis, shannon_fano_coding, 'Shannon arithmetic process is substantially the same').

% NOTE: The Fundamental Theorem of Noiseless/Noisy Channels (Capacity C) 
% has NO viable alternatives mentioned; it is presented as a mathematical 
% proof of a physical limit (Natural Law Signature).

% 5. DEPENDENCIES (Counterfactual Analysis)
% ------------------------------------------------------------------------------
% All discrete coding depends on the Entropy measure
affects_constraint(source_entropy, telegraphy_system).
affects_constraint(source_entropy, computing_machine).

% Channel capacity limits the rate of transmission
affects_constraint(channel_capacity, communication_engineering).

% Theoretical bridge: Markoff processes allow modeling of discrete sources
affects_constraint(markoff_process_model, source_entropy).

% 6. INTENT EVIDENCE
% ------------------------------------------------------------------------------
% Symmetric benefits for the whole of engineering (Coordination/Natural Law)
intent_beneficiary_class(information_theory_genesis, communication_engineering).
intent_power_change(information_theory_genesis, communication_engineering, 0.90).

% No asymmetric capture detected; the theory serves the "engineering problem" 
% regardless of the "semantic aspects."

% 7. RECOMMENDATIONS
% ------------------------------------------------------------------------------
recommendation(rec1, 'Adopt the bit (binary digit) as the universal unit of measure').
affects_constraint(rec1, logarithmic_measure).

recommendation(rec2, 'Use Markoff processes to approximate natural languages for encoding efficiency').
affects_constraint(rec2, source_entropy).

% 8. OMEGA VARIABLES
% ------------------------------------------------------------------------------
omega_variable(ov1, conceptual, 'Precise human perception (fidelity evaluation) for speech/music is implicitly known but not yet explicitly modeled').
