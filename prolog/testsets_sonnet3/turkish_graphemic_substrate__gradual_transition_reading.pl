% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__gradual_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__gradual_transition_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: turkish_graphemic_substrate__gradual_transition_reading
 *   human_readable: Managed Dual-Script Transition Reading of the Turkish Graphemic Substrate
 *   domain: political_linguistics/state_formation/cultural_engineering
 *
 * SUMMARY:
 *   This story instantiates the gradual-transition reading of the contested
 *   Turkish graphemic substrate kernel: the claim that Arabic-script and
 *   Latin-script literacy can coexist for a bounded 5-15 year window,
 *   sequencing state modernization against intergenerational knowledge
 *   preservation. This is one of three structurally distinct readings of the
 *   same kernel — the ottoman_continuity_reading (indefinite Arabic-script
 *   legitimacy) and the secular_nationalist_reading (immediate Latin-script
 *   cutover) are separate constraints with their own epsilon values and
 *   stakeholder structures, not alternative measurements of this one. Under
 *   this reading's own lights, the standing arrangement under contest is the
 *   phased-coexistence administrative apparatus itself — the dual literacy
 *   campaigns, dual print runs, and staged registry conversions — not either
 *   flanking endpoint.
 *
 * KEY AGENTS:
 *   - older_literate_generation: primary beneficiary (moderate/constrained) — retains functional literacy without abrupt disruption
 *   - religious_and_legal_scribes: beneficiary and partial agenda_setter (organized/constrained) — professional relevance preserved through the transition window
 *   - younger_school_age_cohort: primary payer (powerless/trapped) — bears dual-instruction cost with no voice in the schedule
 *   - transitional_state_administrators: agenda_setter (institutional/arbitrage) — designs and enforces the phase-out pace
 *   - secular_nationalist_reform_faction: excluded (powerful/constrained) — sees the transition window as diluting the intended civilizational rupture
 *   - ottoman_continuity_advocates: excluded (organized/constrained) — sees the declared sunset as foreclosing permanence they would prefer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__gradual_transition_reading, 0.42).
domain_priors:suppression_score(turkish_graphemic_substrate__gradual_transition_reading, 0.35).
domain_priors:theater_ratio(turkish_graphemic_substrate__gradual_transition_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__gradual_transition_reading, scaffold).
narrative_ontology:human_readable(turkish_graphemic_substrate__gradual_transition_reading, "Managed Dual-Script Transition Reading of the Turkish Graphemic Substrate").
narrative_ontology:topic_domain(turkish_graphemic_substrate__gradual_transition_reading, "political_linguistics/state_formation/cultural_engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__gradual_transition_reading).
narrative_ontology:has_sunset_clause(turkish_graphemic_substrate__gradual_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__gradual_transition_reading, '34925084-c73b-40d6-9cc1-eef501b8a550').
narrative_ontology:cs_kernel_codification('34925084-c73b-40d6-9cc1-eef501b8a550', distributed).
narrative_ontology:cs_authority_grounding('34925084-c73b-40d6-9cc1-eef501b8a550', extraction).
narrative_ontology:cs_interpretation_layer_present('34925084-c73b-40d6-9cc1-eef501b8a550').
narrative_ontology:cs_reading_relation('34925084-c73b-40d6-9cc1-eef501b8a550', turkish_graphemic_substrate__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('34925084-c73b-40d6-9cc1-eef501b8a550', turkish_graphemic_substrate__secular_nationalist_reading, influences).
narrative_ontology:cs_axiom('34925084-c73b-40d6-9cc1-eef501b8a550', foundational, bounded_transition_legitimacy).
narrative_ontology:cs_axiom_status(bounded_transition_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('34925084-c73b-40d6-9cc1-eef501b8a550', bounded_transition_legitimacy, instrumental).
narrative_ontology:cs_axiom('34925084-c73b-40d6-9cc1-eef501b8a550', foundational, intergenerational_continuity_outweighs_immediate_rupture).
narrative_ontology:cs_axiom_status(intergenerational_continuity_outweighs_immediate_rupture, holdable).
narrative_ontology:cs_axiom_grounding('34925084-c73b-40d6-9cc1-eef501b8a550', intergenerational_continuity_outweighs_immediate_rupture, conventional).
narrative_ontology:cs_reference_frame('34925084-c73b-40d6-9cc1-eef501b8a550', phased_administrative_sequencing).
narrative_ontology:cs_drift_state('34925084-c73b-40d6-9cc1-eef501b8a550', post_transition_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('34925084-c73b-40d6-9cc1-eef501b8a550', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, older_literate_generation).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, religious_and_legal_scribes).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, rural_populations).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, transitional_state_administrators).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, younger_school_age_cohort).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, printing_and_publishing_industry).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, state_literacy_campaign_planners).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__gradual_transition_reading, gradualist_reform_doctrine).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__gradual_transition_reading, intergenerational_continuity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Already literate in Arabic script; a managed transition lets them continue reading correspondence, religious texts, and legal documents without being made functionally illiterate overnight. They gain time to acquire Latin literacy at their own pace, or not at all, without immediate social exclusion.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, older_literate_generation, beneficiary,
    moderate, biographical, constrained, national).

% Their professional standing depends on Arabic-script literacy for religious, legal, and clerical documents. A managed coexistence period preserves their institutional relevance and gives them leverage in negotiating the pace and terms of transition, rather than facing abrupt obsolescence.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, religious_and_legal_scribes, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__gradual_transition_reading, religious_and_legal_scribes, agenda_setter).

% Have the least access to new schools, printed materials, and literacy campaigns. A slower transition reduces the risk that they are cut off entirely from written communication during the changeover, though they remain dependent on whatever pace the state actually delivers.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, rural_populations, beneficiary,
    powerless, biographical, trapped, regional).

% Design and enforce the phased schedule — which documents, schools, and registries convert when. They can tune the pace to manage unrest and administrative capacity, and they answer to political leadership pushing for faster secular-nationalist consolidation.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, transitional_state_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Educated under a curriculum straddling two scripts, absorbing the cost of dual literacy instruction, delayed full fluency in either system, and uncertainty about which script will actually govern their adult working lives. They have no voice in setting the schedule that shapes their schooling.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, younger_school_age_cohort, payer,
    powerless, generational, trapped, national).

% Must maintain dual typesetting, dual print runs, and dual distribution for the length of the transition, absorbing costs that a sharp cutover would avoid. Cannot fully commit capital to either script's infrastructure until the transition's endpoint is certain.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, printing_and_publishing_industry, payer,
    moderate, biographical, constrained, national).

% Bear the administrative and fiscal burden of running two parallel literacy infrastructures — teacher training, textbooks, adult education — rather than a single decisive campaign. Face criticism from nationalist reformers that the slower pace dilutes the modernization mandate.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, state_literacy_campaign_planners, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__gradual_transition_reading, state_literacy_campaign_planners, agenda_setter).

% Would prefer an immediate, total cutover to Latin script as a decisive break from the Ottoman-Islamic past; they view the gradual coexistence reading as a compromise that dilutes the symbolic force of rupture and gives conservative institutions time to entrench resistance. Their preferred timeline is not the one enacted here.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, secular_nationalist_reform_faction, excluded,
    powerful, civilizational, constrained, national).

% Would prefer indefinite retention of Arabic script as the legitimate graphemic substrate tied to Ottoman-Islamic civilizational continuity; they view even a managed transition as a one-way ratchet toward eventual full displacement, and their preferred permanence is foreclosed by the transition's declared endpoint.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, ottoman_continuity_advocates, excluded,
    organized, civilizational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(turkish_graphemic_substrate__gradual_transition_reading, diffuse).
narrative_ontology:fixing_cost_class(turkish_graphemic_substrate__gradual_transition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem of converting an entire population's written communication system without severing the older generation, clergy, and legal apparatus from functional literacy overnight — sequencing conversion so administrative, educational, and print infrastructure can be built out in step with demand.
% TRANSFER_FUNCTION: Moves administrative certainty and civilizational-rupture symbolism away from the secular-nationalist faction (who wanted immediate cutover) and moves absorbable-but-real cost — dual instruction time, dual print runs, uncertain long-run literacy outcomes — onto the younger cohort and the industries and planners who must run two systems at once.
% ABSENT_VOICES: Both flanking factions are excluded from setting this reading's terms: secular nationalists who wanted an immediate break, and Ottoman continuity advocates who wanted no legally-sunsetted endpoint at all. Neither is in the room shaping the 5-15 year window; the window is set by administrators managing unrest and capacity, not by either ideological camp.
% DISAPPEARANCE_RATIONALE: If the managed-coexistence arrangement vanished, the state would have to choose between the secular-nationalist immediate-cutover reading or an open-ended Ottoman-continuity-style permanence — either would eliminate the dual-script schools, dual print runs, and phased registry conversions that currently structure how millions learn to read and how the printing industry allocates capital.
% FOUNDING_PROBLEM: A single generation faced simultaneous demands: modernize the writing system to align with a new national project while not making an entire pre-existing literate population, clerical class, and legal archive instantly illegible.
% FOUNDING_PROBLEM_CORROBORATION: Transitional administrators and religious/legal scribes attest the problem remains live — full transition is not administratively or socially complete. Secular-nationalist reformers, an outside faction with no stake in prolonging coexistence, attest the founding problem is largely solved and that continued dual-script accommodation now mainly protects clerical and print-industry interests rather than serving genuine transitional need.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__gradual_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__gradual_transition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__gradual_transition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__gradual_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__gradual_transition_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__gradual_transition_reading_tests).
:- end_tests(turkish_graphemic_substrate__gradual_transition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rises modestly across the interval as the transition matures and its costs — dual-track schooling, dual print infrastructure — accumulate on the younger cohort and industry rather than dissipate. Suppression is comparatively low (0.35) because this reading's coordination function is genuine: it is not primarily coercing an alternative into silence but managing a real sequencing problem, though the state does actively enforce the phase schedule against faster or slower alternatives pressed by the flanking factions. Theater ratio is modest (0.28) and rises slightly as some dual-script provisions likely persist past their functional necessity into symbolic accommodation of scribal and clerical interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries — the already-literate older generation, the scribal class, and under-resourced rural populations — are positioned as low-d agents: the arrangement subsidizes their continued functioning rather than extracting from them. The younger school-age cohort, the print industry, and the literacy-campaign planners are high-d targets: they absorb the transition's real cost (duplicated instructional time, duplicated capital, extended fiscal burden) without controlling its pace or timeline. Transitional administrators sit as agenda-setters with arbitrage-grade exit — they can adjust policy timing to manage political risk in ways no other seat can.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification is deliberately time-bound: the declared 5-15 year sunset is what prevents this reading from being mistaken for either a permanent Ottoman-continuity arrangement or a permanent secular-nationalist cutover. If the coexistence period were extended indefinitely without a genuine sunset, the same structure would decay into a tangled_rope or piton — preserving the scribal class's institutional relevance well past the point where the founding intergenerational-transfer problem is solved. The founding_problem_status is authored as contested precisely because the sunset clause's credibility is the load-bearing fact: administrators and scribes have structural incentive to claim the problem remains live longer than it does.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sunset_credibility_ambiguity,
    'Is the declared 5-15 year transition window a genuine, binding sunset, or a soft target that scribal and administrative interests can extend indefinitely once entrenched?',
    'Track whether dual-script provisions (parallel registries, dual-track curricula, parallel legal documentation) are actually retired on schedule, or whether extensions are granted and on what stated basis.',
    'If the sunset is honored, this reading remains a genuine scaffold. If systematically extended past 15 years without a renewed founding problem, it reclassifies toward tangled_rope or piton — the coordination justification becomes cover for entrenched scribal-class and print-industry interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_credibility_ambiguity, empirical, 'Whether the transition''s declared sunset clause is structurally binding or rhetorically deferrable.').

omega_variable(
    kernel_framing_choice_ambiguity,
    'Is the graphemic substrate kernel best framed as a single contested legitimacy claim (which script IS Turkish identity) with three readings, or as three independent policy proposals that happen to share subject matter?',
    'Examine whether the three readings were historically debated as competing answers to one legitimacy question in the same forum (e.g., the same language commission), versus pursued as separate, non-competing initiatives.',
    'If the readings genuinely competed for the same legitimacy slot, the forecloses/coexists_with/influences relations declared here hold as authored. If they were pursued in separate arenas without direct contest, the kernel framing itself is looser than declared, and the reading_relations should weaken toward coexists_with across the board.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_ambiguity, conceptual, 'Whether the single-kernel framing with three competing readings accurately models the historical contest, or overstates its unity.').

omega_variable(
    younger_cohort_beneficiary_or_victim_ambiguity,
    'Does the younger school-age cohort''s dual-script education impose a net cost (delayed fluency, curricular confusion) or a net benefit (genuine biliteracy, historical-document access) once the transition completes?',
    'Longitudinal literacy outcome studies comparing cohorts educated entirely post-transition against cohorts educated during the dual-script window.',
    'If dual education produces durable biliteracy value, the younger cohort is a delayed beneficiary rather than a pure payer, which would soften this reading''s extractiveness score.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(younger_cohort_beneficiary_or_victim_ambiguity, empirical, 'Whether the transitional generation''s schooling cost nets out as extraction or as a delayed dividend.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__gradual_transition_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(turk_tr_t0, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(turk_tr_t3, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 3, 0.18).
narrative_ontology:measurement(turk_tr_t6, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(turk_tr_t9, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 9, 0.25).
narrative_ontology:measurement(turk_tr_t12, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(turk_tr_t15, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 15, 0.28).

% Extraction over time
narrative_ontology:measurement(turk_be_t0, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(turk_be_t3, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 3, 0.34).
narrative_ontology:measurement(turk_be_t6, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(turk_be_t9, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 9, 0.4).
narrative_ontology:measurement(turk_be_t12, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement(turk_be_t15, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 15, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(turk_su_t0, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(turk_su_t3, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 3, 0.28).
narrative_ontology:measurement(turk_su_t6, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 6, 0.32).
narrative_ontology:measurement(turk_su_t9, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 9, 0.33).
narrative_ontology:measurement(turk_su_t12, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 12, 0.34).
narrative_ontology:measurement(turk_su_t15, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 15, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__gradual_transition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(turkish_graphemic_substrate__gradual_transition_reading, 0.1).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate__ottoman_continuity_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate__secular_nationalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'the Turkish script reform question' per the epsilon-invariance principle. Each reading authors its own epsilon: this gradual_transition_reading is authored at 0.42 (moderate, rising modestly as transition costs accumulate); the secular_nationalist_reading would author a lower epsilon under its own framing (rupture-as-coordination-benefit) but higher suppression; the ottoman_continuity_reading would author a distinct extraction profile centered on preserving clerical/legal monopoly over literacy access. All three link via affects_constraints as members of the same kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
