% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__modernist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__modernist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__modernist_reading
 *   human_readable: Latin Script Reform as Rupture-Legitimation (Modernist Reading)
 *   domain: political_linguistics/state_formation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the orthographic legitimacy
 *   kernel: the modernist reading, under which the legitimacy of a script
 *   reform derives from its capacity to enact rupture from an Ottoman/Islamic
 *   past and alignment with Western/European civilizational markers. Under
 *   this reading the reform is not evaluated for its literacy or
 *   administrative payoff (that is the instrumentalist reading's ε) nor for
 *   what it preserves (the continuity reading's ε) — it is evaluated for
 *   whether it constitutes a decisive break, and that break is what produces
 *   both the reform's legitimacy claim and its extraction. The
 *   Ottoman-trained literate class and religious scholars are rendered
 *   structurally obsolete not as a side effect of pursuing efficiency but as
 *   the constitutive act the reform exists to perform. This ε (0.78) is high
 *   because the rupture framing requires their delegitimation as evidence of
 *   success — a partial, gradual, or reversible transition would fail to
 *   satisfy the modernist reading's own legitimacy criterion.
 *
 * KEY AGENTS:
 *   - modernizing_state_apparatus: agenda_setter, decrees and enforces the script change
 *   - new_republican_elite: beneficiary, converts pre-existing Western-aligned cultural capital into monopoly literacy
 *   - western_aligned_technocrats: beneficiary/agenda_setter, administers and interprets the new order
 *   - ottoman_literate_class: payer, rendered functionally illiterate overnight
 *   - religious_scholars_ulema: payer, loses textual and interpretive authority
 *   - older_generation_citizens: payer, re-illiterated within their own lifetimes
 *   - diaspora_and_foreign_observers: excluded observer, certifies legitimacy without bearing cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, 0.78).
domain_priors:suppression_score(orthographic_legitimacy_kernel__modernist_reading, 0.81).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__modernist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__modernist_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__modernist_reading, "Latin Script Reform as Rupture-Legitimation (Modernist Reading)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__modernist_reading, "political_linguistics/state_formation").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__modernist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__modernist_reading, '6aa2440c-8d4c-4cdc-8dd5-df8787f052fb').
narrative_ontology:cs_kernel_codification('6aa2440c-8d4c-4cdc-8dd5-df8787f052fb', distributed).
narrative_ontology:cs_authority_grounding('6aa2440c-8d4c-4cdc-8dd5-df8787f052fb', extraction).
narrative_ontology:cs_interpretation_layer_present('6aa2440c-8d4c-4cdc-8dd5-df8787f052fb').
narrative_ontology:cs_reading_relation('6aa2440c-8d4c-4cdc-8dd5-df8787f052fb', orthographic_legitimacy_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('6aa2440c-8d4c-4cdc-8dd5-df8787f052fb', orthographic_legitimacy_kernel__instrumentalist_reading, influences).
narrative_ontology:cs_axiom('6aa2440c-8d4c-4cdc-8dd5-df8787f052fb', foundational, national_legitimacy_requires_civilizational_rupture).
narrative_ontology:cs_axiom_status(national_legitimacy_requires_civilizational_rupture, holdable).
narrative_ontology:cs_axiom_grounding('6aa2440c-8d4c-4cdc-8dd5-df8787f052fb', national_legitimacy_requires_civilizational_rupture, conventional).
narrative_ontology:cs_axiom('6aa2440c-8d4c-4cdc-8dd5-df8787f052fb', foundational, script_change_as_constitutive_identity_act).
narrative_ontology:cs_axiom_status(script_change_as_constitutive_identity_act, holdable).
narrative_ontology:cs_axiom_grounding('6aa2440c-8d4c-4cdc-8dd5-df8787f052fb', script_change_as_constitutive_identity_act, instrumental).
narrative_ontology:cs_reference_frame('6aa2440c-8d4c-4cdc-8dd5-df8787f052fb', ottoman_islamic_scriptural_continuity).
narrative_ontology:cs_drift_state('6aa2440c-8d4c-4cdc-8dd5-df8787f052fb', post_republican_founding_decree, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('6aa2440c-8d4c-4cdc-8dd5-df8787f052fb', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, new_republican_elite).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, western_aligned_technocrats).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_class).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, religious_scholars_ulema).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, older_generation_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, younger_generation_citizens).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, civilizational_westward_reorientation_doctrine).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, national_rupture_from_ottoman_past).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates the alphabet change by decree, staffs the literacy campaigns that teach only the new script, and controls what counts as an official document going forward. Draws legitimacy from being seen as the agent of civilizational realignment; the reform is presented as inseparable from the state's founding claim to modern nationhood.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Already educated abroad or in reform-aligned institutions, this group reads and writes the new script fluently from the outset. The reform converts their pre-existing cultural capital into a durable monopoly on literacy, administration, and interpretation of law and history going forward.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, new_republican_elite, beneficiary,
    powerful, generational, mobile, national).

% Design and implement the reform, positioning themselves as the indispensable interpreters of the new order — publishers, textbook authors, and school inspectors whose expertise is manufactured by the same rupture they administer. Their institutional standing depends on the modernist framing remaining official doctrine.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, western_aligned_technocrats, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__modernist_reading, western_aligned_technocrats, agenda_setter).

% Scribes, clerks, and professionals whose entire trained literacy is rendered obsolete overnight. They cannot read new official documents, cannot easily retrain in middle age, and lose administrative and social standing that took a lifetime to build. There is no exit — the state simply stops recognizing their competence.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_class, payer,
    moderate, biographical, trapped, national).

% Their authority rested on interpretive mastery of texts in the old script and its connection to religious and legal tradition. The reform severs the population's direct access to that textual tradition, transferring interpretive authority to secular, state-credentialed institutions. They can resist rhetorically but cannot reverse the state's monopoly on official script.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, religious_scholars_ulema, payer,
    organized, civilizational, constrained, national).

% Ordinary literate adults who learned to read in the old script are functionally re-illiterated by the change. Newspapers, signage, and government forms shift beneath them; their children are schooled in a script they themselves cannot read, inverting the ordinary flow of literate authority within families.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, older_generation_citizens, payer,
    powerless, biographical, trapped, national).

% Educated exclusively in the new script, they gain full access to the reformed state's institutions and to the modernist narrative of national identity, at the cost of severed direct access to pre-reform textual and religious heritage without specialist mediation.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, younger_generation_citizens, beneficiary,
    moderate, generational, mobile, national).

% Western commentators and allied states largely certify the reform as unambiguous progress and modernization; their approval is part of what makes the rupture framing legible as legitimacy, but they bear none of the transitional costs and are not present to weigh the ulema's or the literate class's account.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, diaspora_and_foreign_observers, excluded,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single national standard of written communication going forward, in principle enabling universal administration, printing, and education under one system rather than fragmented competing scripts.
% TRANSFER_FUNCTION: Moves interpretive authority, administrative standing, and the capacity to be read as 'modern' and legitimate from the Ottoman-trained literate and religious-scholarly classes to the new republican elite and the state apparatus that certifies the new script as the marker of civilizational belonging.
% ABSENT_VOICES: The Ottoman literate class and the ulema are permitted to object rhetorically but are excluded from the actual decree process; foreign observers whose approval legitimizes the rupture bear none of its costs and are not present to weigh dissenting accounts of what was lost.
% DISAPPEARANCE_RATIONALE: If the rupture framing were withdrawn and the state instead treated the old and new scripts as equally legitimate parallel systems, the modernizing elite's monopoly on interpreting official life would erode, the ulema's textual authority would partially recover, and the state's founding legitimacy claim — that the nation is constituted by breaking from the Ottoman/Islamic past — would lose its primary evidentiary anchor.
% FOUNDING_PROBLEM: A new state sought a legitimating identity distinct from the empire it replaced, and needed a visible, irreversible marker that the break was total rather than cosmetic — a marker citizens could not quietly ignore or reverse.
% FOUNDING_PROBLEM_CORROBORATION: The state and its administrative heirs attest the rupture was necessary and remains foundational to national identity. Independent historians and linguists outside the state apparatus, along with descendants of the displaced literate class, attest that literacy and administrative efficiency gains could have been achieved without the wholesale delegitimation of the prior script and its associated class — supporting a reading that the rupture framing exceeded any coordination need.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__modernist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__modernist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__modernist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__modernist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__modernist_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.78) and rising across the interval because the modernist reading's legitimacy is directly proportional to how thoroughly the old literate order is displaced — the ε trajectory tracks the depth of the rupture being institutionalized, not merely literacy uptake (which would be the instrumentalist story's metric). Suppression starts very high (0.85) reflecting the initial decree-and-enforcement phase (banning old-script print, purging old-script officials) and eases only slightly as the new generation ages into dominance and active coercion becomes less necessary — normalization substitutes for enforcement. Theater ratio rises over time (0.20 to 0.42) as the reform's ongoing justification increasingly rests on retrospective national-identity narrative rather than the original literacy-crisis rationale, consistent with a coordination story whose founding problem (per six_questions) is contested as dead while the arrangement persists.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (state apparatus, republican elite, technocrats) hold institutional/organized power with mobile or arbitrage exit — the reform converts their pre-existing capital into permanent advantage, so directionality sits near the subsidized end. Victims (Ottoman literate class, ulema, older citizens) are trapped or constrained — their entire skill/authority base is invalidated by fiat with no retraining window, pushing directionality toward full target. Younger citizens are coded beneficiary because they are onboarded natively into the new system, even though they bear the civilizational-severance cost that the continuity reading would flag as extractive — that divergence is exactly what makes this a distinct reading rather than a restatement of continuity_reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored contested rather than dead or live outright: the state's own account treats the rupture as permanently, foundationally necessary (a live civilizational project), while outside historians treat the literacy/administrative problem as solved decades ago, leaving the rupture framing as a persisting identity-legitimation exercise no longer doing coordination work. This is the tangled_rope signature — genuine early coordination benefit (single national script) coexists with an enforcement apparatus and beneficiary class whose standing depends on maintaining the rupture narrative long after any administrative necessity would require it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_necessity_vs_construction,
    'Was the total rupture from Ottoman/Islamic script genuinely necessary to achieve modernization and national cohesion, or was rupture itself the goal, with literacy/administration serving as its instrumental justification?',
    'Comparative study of contemporaneous script/orthography reforms elsewhere that achieved literacy and administrative gains via gradual or parallel-script transitions (retaining reading access to prior tradition) versus this case''s abrupt total replacement; degree of gain differential would indicate how much of the ε is attributable to rupture-for-its-own-sake versus instrumental necessity.',
    'If gradual/parallel transitions elsewhere achieved comparable literacy gains without comparable delegitimation of the prior literate class, this reading''s high ε is validated as reading-specific to the rupture goal rather than an artifact of unavoidable transition costs — strengthening the tangled_rope classification over a pure rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_necessity_vs_construction, empirical, 'Whether rupture was necessary for modernization or was itself the sought outcome.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the modernist reading a genuinely distinct legitimacy claim, or is it better understood as the instrumentalist reading''s post-hoc civilizational narrative layered onto the same underlying policy?',
    'Examine founding-era state rhetoric and archival policy debate: if administrative/literacy justifications and civilizational-rupture justifications were argued interchangeably by the same actors without distinction, the two readings may share more structural overlap than the kernel model assumes; if they were argued by distinguishable factions with different stakes, the readings are genuinely separable.',
    'If the two readings collapse into one in the historical record, this would argue for merging modernist_reading and instrumentalist_reading into a single constraint rather than maintaining them as siblings — a direct test of the ε-invariance decomposition choice made here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the modernist and instrumentalist readings are structurally separable or artifacts of retrospective narrative construction.').

omega_variable(
    ulema_authority_displacement_completeness,
    'Did the ulema''s interpretive authority genuinely depend on script literacy, or did it survive in oral, institutional, and social forms that the orthographic reform did not touch?',
    'Track post-reform religious authority structures: persistence of oral religious education, informal script literacy retention within religious institutions, and continued social deference to religious scholars independent of new-script competence.',
    'If religious authority persisted substantially through non-textual channels, the victim-side ε for religious_scholars_ulema is overstated in this reading; if authority collapsed in step with script literacy, the high ε is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ulema_authority_displacement_completeness, empirical, 'Whether the ulema''s power was contingent on old-script literacy or independently durable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__modernist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(orth_tr_t8, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(orth_tr_t16, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(orth_tr_t24, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(orth_tr_t32, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(orth_tr_t40, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(orth_be_t8, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(orth_be_t16, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 16, 0.75).
narrative_ontology:measurement(orth_be_t24, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 24, 0.77).
narrative_ontology:measurement(orth_be_t32, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 32, 0.78).
narrative_ontology:measurement(orth_be_t40, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(orth_su_t8, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 8, 0.83).
narrative_ontology:measurement(orth_su_t16, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 16, 0.8).
narrative_ontology:measurement(orth_su_t24, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 24, 0.79).
narrative_ontology:measurement(orth_su_t32, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 32, 0.81).
narrative_ontology:measurement(orth_su_t40, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__modernist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__modernist_reading, 0.08).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__instrumentalist_reading).

% DUAL FORMULATION NOTE:
% This is one of three sibling constraints decomposed from the single natural-language 'orthographic legitimacy' claim, per the ε-invariance principle. The modernist_reading (this file) authors high extraction because its legitimacy criterion IS civilizational rupture, making the delegitimation of the prior literate class constitutive rather than incidental. continuity_reading authors extraction against the loss of tradition-access as its own criterion (different victim framing, likely higher ε for cultural/religious continuity loss specifically). instrumentalist_reading authors extraction against literacy/efficiency shortfalls only, plausibly yielding a much lower ε since transitional costs are judged against measurable administrative gains rather than civilizational symbolism. All three readings describe the same historical script-reform event but are structurally distinct constraints with different ε, different vindicated propositions, and different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
