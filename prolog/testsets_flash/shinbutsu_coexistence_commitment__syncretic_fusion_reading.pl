% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__syncretic_fusion_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: shinbutsu_coexistence_commitment__syncretic_fusion_reading
 *   human_readable: Shinbutsu Coexistence: Honji Suijaku Syncretic Fusion
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint story describes the 'syncretic fusion' reading of
 *   Shinbutsu-shugo, specifically the honji suijaku doctrine, where kami are
 *   understood as local manifestations of universal Buddhist truths. This
 *   reading posits a coherent, unified ontology that integrated kami worship
 *   into a Buddhist framework, becoming the dominant religious paradigm in
 *   Japan for over a millennium. The constraint is claimed as a Rope due to
 *   its genuine coordination function in unifying diverse religious
 *   practices, but its metrics reflect a degree of extraction and suppression
 *   inherent in the hierarchical integration.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.2).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.4).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Shinbutsu Coexistence: Honji Suijaku Syncretic Fusion").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious_studies/philosophy_of_religion/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '90385787-8af5-44c5-8c18-3a146906d15c').
narrative_ontology:cs_kernel_codification('90385787-8af5-44c5-8c18-3a146906d15c', formalized).
narrative_ontology:cs_authority_grounding('90385787-8af5-44c5-8c18-3a146906d15c', lineage).
narrative_ontology:cs_interpretation_layer_present('90385787-8af5-44c5-8c18-3a146906d15c').
narrative_ontology:cs_reading_relation('90385787-8af5-44c5-8c18-3a146906d15c', shinbutsu_coexistence_commitment__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('90385787-8af5-44c5-8c18-3a146906d15c', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('90385787-8af5-44c5-8c18-3a146906d15c', foundational, kami_as_buddhist_manifestations).
narrative_ontology:cs_axiom_status(kami_as_buddhist_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('90385787-8af5-44c5-8c18-3a146906d15c', kami_as_buddhist_manifestations, theological).
narrative_ontology:cs_axiom('90385787-8af5-44c5-8c18-3a146906d15c', foundational, universal_buddhist_truth).
narrative_ontology:cs_axiom_status(universal_buddhist_truth, holdable).
narrative_ontology:cs_axiom_grounding('90385787-8af5-44c5-8c18-3a146906d15c', universal_buddhist_truth, theological).
narrative_ontology:cs_reference_frame('90385787-8af5-44c5-8c18-3a146906d15c', honji_suijaku_orthodoxy).
narrative_ontology:cs_drift_state('90385787-8af5-44c5-8c18-3a146906d15c', meiji_restoration_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('90385787-8af5-44c5-8c18-3a146906d15c', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_court).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_elites).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kami_worshippers).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_universalism).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kami_as_local_manifestations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and beneficiaries of the honji suijaku doctrine, which elevated Buddhist cosmology while integrating local kami. They administered jinguji (shrine-temple complexes) and collected offerings, solidifying their authority over both Buddhist and kami worship.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_clergy, agenda_setter,
    institutional, generational, constrained, national).

% Benefited from the syncretic framework by integrating local religious practices into a broader, more unified religious system that supported imperial authority and legitimacy. The fusion provided a coherent theological basis for their rule.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_court, beneficiary,
    institutional, generational, constrained, national).

% Local elites, including shrine priests and powerful families, often adopted the honji suijaku framework to legitimize their local kami cults within the dominant Buddhist cosmology, gaining prestige and resources through association with powerful Buddhist temples.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_elites, beneficiary,
    organized, biographical, mobile, local).

% While benefiting from a richer spiritual framework, they were increasingly directed to Buddhist rituals and interpretations for their kami worship, often paying fees or offerings to Buddhist institutions that now managed their local shrines. Their identity was deeply tied to local kami, making exit from the syncretic system difficult.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kami_worshippers, payer,
    powerless, biographical, identity_locked, local).

% Advocated for the purity and independence of kami worship, rejecting the subordination of kami to Buddhist deities. Their voices were largely marginalized during the peak of honji suijaku, as the dominant theological and institutional structures favored fusion.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinto_purists, excluded,
    moderate, generational, constrained, national).

% Analyze the historical development and impact of shinbutsu-shugo, evaluating the coherence and political functions of the honji suijaku doctrine. They assess the evidence for genuine theological fusion versus institutional power dynamics.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, modern_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a coherent theological framework that integrated diverse local kami cults into a universal Buddhist cosmology, facilitating religious harmony and administrative control across Japan.
% TRANSFER_FUNCTION: Transferred spiritual authority and material resources (offerings, land) from local kami cults to Buddhist institutions, in exchange for theological legitimacy and integration into a broader religious system.
% ABSENT_VOICES: Shinto purists and those who maintained a distinct, non-Buddhist understanding of kami were largely excluded from the dominant discourse, their perspectives suppressed by the prevailing syncretic orthodoxy and institutional power of the Buddhist clergy.
% DISAPPEARANCE_RATIONALE: If the honji suijaku doctrine vanished overnight during its peak, the entire religious landscape of Japan would have been thrown into disarray. The institutional structure of jinguji would collapse, the theological justification for many practices would disappear, and the relationship between local and national religious authority would fundamentally shift, leading to widespread reorganization of religious life and power.
% FOUNDING_PROBLEM: The challenge of integrating indigenous Japanese kami worship with the newly introduced, universalizing Buddhist tradition, which presented a potential conflict of religious authority and worldview.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist theological texts and historical records from the period attest to the problem of reconciling the two traditions. Modern historians, from outside the benefiting parties, corroborate that this was a genuine theological and political challenge that honji suijaku aimed to resolve, even if its resolution involved power dynamics.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__syncretic_fusion_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).
:- end_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.2) as the system primarily coordinated religious practices, but Buddhist institutions did accrue significant resources and authority. Suppression is also moderate (0.4) because alternative interpretations (like Shinto purism) were marginalized, and local kami worship was often subordinated. Theater ratio is low (0.1) as the theological and institutional functions were largely genuine and effective for centuries. The temporal measurements show a gradual increase in extractiveness and suppression as the system matured and Buddhist institutions consolidated power, with a slight decline towards the end of the period as challenges began to emerge.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Buddhist clergy and imperial court, this was a highly effective Rope, providing a stable and coherent religious system. For kami worshippers, it was a more ambiguous experience, offering spiritual integration but also imposing new costs and subordinating their traditions. Shinto purists would have experienced it as a Snare, actively suppressing their distinct worldview.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist clergy and imperial court are clear beneficiaries, gaining authority and legitimacy. Local elites also benefited by integrating their local cults. Kami worshippers are payers, bearing the costs of this integration through offerings and doctrinal subordination. Shinto purists are excluded, their alternative views suppressed. Modern historians are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (integrating kami and Buddhism) remained live for centuries. The classification as Rope acknowledges its genuine coordination function, preventing mislabeling it as pure extraction, while the metrics and stakeholder analysis reveal the asymmetric costs and suppression involved in maintaining that 'coordination' from certain seats. The 'contested' status of the founding problem reflects the ongoing debate about whether the integration was truly a solution or a power grab.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_fusion_vs_power_play,
    'To what extent was honji suijaku a genuine theological synthesis, versus a strategic move by Buddhist institutions to absorb and control indigenous kami worship?',
    'Detailed textual analysis of theological debates and institutional records, combined with archaeological evidence of shrine-temple complex development, to trace the motivations and outcomes of syncretic practices.',
    'If primarily a power play, the constraint''s effective extractiveness and suppression would be higher, pushing it closer to a Tangled Rope or Snare from the perspective of kami worshippers. If a genuine synthesis, the Rope classification holds more strongly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_fusion_vs_power_play, conceptual, 'Ambiguity between theological synthesis and institutional power consolidation.').

omega_variable(
    meiji_restoration_impact,
    'How would the classification of this constraint change if analyzed from the perspective of the Meiji Restoration''s Shinbutsu Bunri (separation of kami and Buddhas) policies?',
    'Analyzing the post-1868 period as a separate constraint story, focusing on the new policies'' impact on the existing syncretic structures and the emergence of new forms of religious organization.',
    'The Meiji period would likely see this constraint (syncretic fusion) as a Piton or Snare that needed dismantling, with its extractiveness and suppression becoming explicit targets of state policy. This would highlight the constructed nature of the ''natural'' fusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_restoration_impact, empirical, 'Impact of state-mandated religious separation on the perceived nature of shinbutsu-shugo.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint truly a ''syncretic_fusion_reading'' or is it better understood as a ''domain_partition_reading'' or ''incoherent_bundle_reading''?',
    'Further historical and theological research into the lived experience of practitioners and the explicit doctrinal statements of various schools, focusing on whether a unified ontology was consistently articulated and understood, or if separate domains or deliberate ambiguity were more prevalent.',
    'If reclassified as ''domain_partition_reading'', the extractiveness and suppression would likely be lower, as the two traditions would be seen as coexisting without one subordinating the other. If ''incoherent_bundle_reading'', the constraint would be seen as a Snare or Piton, maintained by power rather than coherent doctrine, with much higher effective extraction and theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity in the core interpretation of Shinbutsu-shugo''s ontological relationship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 700, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t700, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 700, 0.05).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 900, 0.08).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1500, 0.12).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1868, 0.1).

% Extraction over time
narrative_ontology:measurement(shin_be_t700, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 700, 0.1).
narrative_ontology:measurement(shin_be_t900, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 900, 0.15).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1200, 0.2).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1500, 0.25).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1868, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t700, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 700, 0.2).
narrative_ontology:measurement(shin_su_t900, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 900, 0.3).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1200, 0.4).
narrative_ontology:measurement(shin_su_t1500, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1500, 0.45).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1868, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'shinbutsu_coexistence_commitment' kernel. This 'syncretic_fusion_reading' posits an ontological unification of kami and Buddhist deities through honji suijaku, contrasting with the 'domain_partition_reading' (separate domains) and 'incoherent_bundle_reading' (deliberate ambiguity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
