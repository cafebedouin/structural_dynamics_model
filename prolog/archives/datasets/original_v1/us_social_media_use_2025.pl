% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: US Social Media Use 2025
% Domain: Digital Sociology - Social Media Adoption and Platform Dominance
% ==========================================================

% --- 1. Entities & Intervals ---
entity(pew_research_center, organizational).
entity(youtube_platform, individual).
entity(tiktok_platform, individual).
entity(meta_platforms, organizational).
entity(us_adult_users, class).
entity(hispanic_adult_cohort, class).
entity(platform_economy_structure, structural).

interval(adoption_growth_cycle, 2023, 2025).

% --- 2. Events ---
event(ev01_youtube_saturation, measurement, 2025, [target(youtube_platform), reach(0.94)]).
event(ev02_tiktok_surge, transformation, 2025, [target(tiktok_platform), growth_from(0.33), reach(0.38)]).
event(ev03_whatsapp_demographic_skew, observation, 2025, [target(hispanic_adult_cohort), reach(0.58)]).
event(ev04_facebook_stagnation, state_stabilization, 2025, [target(meta_platforms), reach(0.67)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The absolute physical/cognitive limit of human attention and time-budget.
constraint_claim(attention_scarcity_limit, mountain).
constraint_metric(attention_scarcity_limit, accessibility_collapse, 0.95).

% Snare: The social obligation and 'fear of missing out' (FOMO). It tightens as network effects grow.
constraint_claim(network_effect_coercion, snare).
constraint_metric(network_effect_coercion, stakes_inflation, 0.85).

% Piton: The 'Public Square' ideal. A concept of open democratic discourse that persists despite algorithmic curation.
constraint_claim(digital_public_square_myth, piton).
constraint_metric(digital_public_square_myth, suppression, 0.70).

% Rope: Privacy Terms and Conditions. Ties user behavior to rigid platform legal frameworks.
constraint_claim(algorithmic_governance_binding, rope).
constraint_metric(algorithmic_governance_binding, suppression, 0.60).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Adopt interoperable data standards to bypass platform-specific network lock-in.').
recommendation(rec02, 'Implement algorithmic transparency mandates to reduce involuntary user suppression.').

affects_constraint(rec01, network_effect_coercion).
affects_constraint(rec02, algorithmic_governance_binding).

veto_actor(meta_platforms).
veto_actor(platform_economy_structure).

veto_exposed(meta_platforms, rec01).
veto_exposed(platform_economy_structure, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (2023 Initial State)
measurement(m01, us_adult_users, accessibility_collapse(individual), 2023, 0.35).
measurement(m02, us_adult_users, stakes_inflation(individual), 2023, 0.45).
measurement(m03, us_adult_users, suppression(individual), 2023, 0.25).
measurement(m04, us_adult_users, resistance(individual), 2023, 0.30).

measurement(m05, pew_research_center, accessibility_collapse(organizational), 2023, 0.10).
measurement(m06, pew_research_center, stakes_inflation(organizational), 2023, 0.20).
measurement(m07, pew_research_center, suppression(organizational), 2023, 0.05).
measurement(m08, pew_research_center, resistance(organizational), 2023, 0.05).

measurement(m09, hispanic_adult_cohort, accessibility_collapse(class), 2023, 0.40).
measurement(m10, hispanic_adult_cohort, stakes_inflation(class), 2023, 0.50).
measurement(m11, hispanic_adult_cohort, suppression(class), 2023, 0.30).
measurement(m12, hispanic_adult_cohort, resistance(class), 2023, 0.20).

measurement(m13, platform_economy_structure, accessibility_collapse(structural), 2023, 0.05).
measurement(m14, platform_economy_structure, stakes_inflation(structural), 2023, 0.15).
measurement(m15, platform_economy_structure, suppression(structural), 2023, 0.00). % Beneficiary Logic
measurement(m16, platform_economy_structure, resistance(structural), 2023, 0.00). % Beneficiary Logic

% Time Tn (2025 Current State)
measurement(m17, us_adult_users, accessibility_collapse(individual), 2025, 0.72). % Saturation of use
measurement(m18, us_adult_users, stakes_inflation(individual), 2025, 0.90). % Essential for social/economic life
measurement(m19, us_adult_users, suppression(individual), 2025, 0.85). % Algorithmic curation dominance
measurement(m20, us_adult_users, resistance(individual), 2025, 0.15). % Declining opting-out ability

measurement(m21, pew_research_center, accessibility_collapse(organizational), 2025, 0.05).
measurement(m22, pew_research_center, stakes_inflation(organizational), 2025, 0.95).
measurement(m23, pew_research_center, suppression(organizational), 2025, 0.10).
measurement(m24, pew_research_center, resistance(organizational), 2025, 0.98).

measurement(m25, hispanic_adult_cohort, accessibility_collapse(class), 2025, 0.10). % High platform engagement
measurement(m26, hispanic_adult_cohort, stakes_inflation(class), 2025, 0.95). % Dependency on WhatsApp/TikTok
measurement(m27, hispanic_adult_cohort, suppression(class), 2025, 0.40).
measurement(m28, hispanic_adult_cohort, resistance(class), 2025, 0.60).

measurement(m29, platform_economy_structure, accessibility_collapse(structural), 2025, 0.00).
measurement(m30, platform_economy_structure, stakes_inflation(structural), 2025, 0.40).
measurement(m31, platform_economy_structure, suppression(structural), 2025, 0.00). % Beneficiary Logic
measurement(m32, platform_economy_structure, resistance(structural), 2025, 0.00). % Beneficiary Logic

% --- 6. Intent Evidence ---
intent_viable_alternative(adoption_growth_cycle, platform_economy_structure, "Decentralized_Open_Source_Social_Protocols").
intent_alternative_rejected(adoption_growth_cycle, platform_economy_structure, "Decentralized_Open_Source_Social_Protocols").

intent_beneficiary_class(adoption_growth_cycle, meta_platforms).
intent_power_change(adoption_growth_cycle, meta_platforms, 0.75). % Massive structural capture of US attention

intent_loser_class(adoption_growth_cycle, us_adult_users). % Loss of autonomous attention and data privacy
intent_power_change(adoption_growth_cycle, us_adult_users, -0.65).

intent_suppression_level(adoption_growth_cycle, meta_platforms, structural, 0.0).
intent_resistance_level(adoption_growth_cycle, meta_platforms, structural, 0.0).

intent_norm_strength(adoption_growth_cycle, 2023, 0.80). % High social pressure
intent_norm_strength(adoption_growth_cycle, 2025, 0.98). % Absolute standard for digital citizenship
