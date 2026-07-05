% ============================================================================
% CONSTRAINT STORY: border_legitimacy__humanitarian_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__humanitarian_obligation_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: border_legitimacy__humanitarian_obligation_reading
 *   human_readable: Refugee/Non-Refugee Categorical Admission Obligation
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This story instantiates the humanitarian-obligation reading of the
 *   border_legitimacy kernel: states owe admission to persons fleeing
 *   persecution or (in some formulations) disaster, but not to general
 *   economic migrants. This reading is deliberately generated in isolation
 *   from its siblings — sovereignty_reading (border authority as an incident
 *   of territorial sovereignty) and freedom_of_movement_reading (movement as
 *   a presumptive right against which borders must justify themselves). Each
 *   reading is its own constraint with its own ε, beneficiary/victim
 *   structure, and classification; they are linked only through network edges
 *   and the kernel_context note, per the ε-invariance principle. This
 *   reading's structural signature is the bifurcated victim set the
 *   persecution/economic line produces: recognized refugees and misclassified
 *   asylum seekers sit on one side of an administrable but imperfectly
 *   applied test, while economic migrants and climate-displaced persons —
 *   often facing comparable severity of harm — sit permanently outside the
 *   obligation's scope regardless of need.
 *
 * KEY AGENTS:
 *   - recognized_refugees: primary intended beneficiary (powerless/trapped) — receives protection only if classification succeeds
 *   - economic_migrants: primary categorical victim (powerless/trapped) — excluded by definition regardless of severity of hardship
 *   - climate_displaced_without_persecution_nexus: structural victim (powerless/trapped) — the convention's persecution requirement cannot recognize their claim by design
 *   - destination_state_governments: agenda-setter (institutional/arbitrage) — administers and benefits from the bounded scope of obligation
 *   - unhcr_and_asylum_bureaucracies: institutional beneficiary/co-administrator — mandate legitimacy tied to category's continued administrability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.52).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.58).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Refugee/Non-Refugee Categorical Admission Obligation").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, 'e679dc21-5f92-47eb-8903-e6797a459da6').
narrative_ontology:cs_kernel_codification('e679dc21-5f92-47eb-8903-e6797a459da6', formalized).
narrative_ontology:cs_authority_grounding('e679dc21-5f92-47eb-8903-e6797a459da6', lineage).
narrative_ontology:cs_interpretation_layer_present('e679dc21-5f92-47eb-8903-e6797a459da6').
narrative_ontology:cs_reading_relation('e679dc21-5f92-47eb-8903-e6797a459da6', border_legitimacy__sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('e679dc21-5f92-47eb-8903-e6797a459da6', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_axiom('e679dc21-5f92-47eb-8903-e6797a459da6', foundational, persecution_nexus_grounds_obligation).
narrative_ontology:cs_axiom_status(persecution_nexus_grounds_obligation, holdable).
narrative_ontology:cs_axiom_grounding('e679dc21-5f92-47eb-8903-e6797a459da6', persecution_nexus_grounds_obligation, deontological).
narrative_ontology:cs_axiom('e679dc21-5f92-47eb-8903-e6797a459da6', secondary, bounded_obligation_preserves_state_discretion).
narrative_ontology:cs_axiom_status(bounded_obligation_preserves_state_discretion, holdable).
narrative_ontology:cs_axiom_grounding('e679dc21-5f92-47eb-8903-e6797a459da6', bounded_obligation_preserves_state_discretion, conventional).
narrative_ontology:cs_reference_frame('e679dc21-5f92-47eb-8903-e6797a459da6', convention_persecution_nexus_standard).
narrative_ontology:cs_drift_state('e679dc21-5f92-47eb-8903-e6797a459da6', contemporary_climate_displacement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e679dc21-5f92-47eb-8903-e6797a459da6', '').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, recognized_refugees).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, host_state_publics).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, unhcr_and_asylum_bureaucracies).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, climate_displaced_without_persecution_nexus).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, asylum_seekers_misclassified_at_screening).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, asylum_seekers_misclassified_at_screening).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have fled persecution meeting the 1951 Convention definition. If successfully classified as such, they receive a legal admission pathway, non-refoulement protection, and eventual status. Their benefit depends entirely on passing a screening process they do not control and cannot appeal on equal footing with the state's resources.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, recognized_refugees, beneficiary,
    powerless, biographical, trapped, national).

% Flee severe poverty, wage collapse, or slow-onset livelihood destruction that does not fit the persecution nexus. They face the same borders and often the same journeys and smuggling risks as recognized refugees but are categorically excluded from the admission obligation, deportable on discovery, and denied the legal standing to contest exclusion on humanitarian grounds equivalent to asylum claims.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, economic_migrants, payer,
    powerless, biographical, trapped, global).

% Displaced by disasters, sea-level rise, or ecological collapse with no persecuting agent to name. The convention's persecution requirement structurally cannot recognize their claim regardless of severity, leaving them in the same excluded category as economic migrants despite comparable or greater life-threat.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, climate_displaced_without_persecution_nexus, payer,
    powerless, generational, trapped, global).

% Have genuine persecution claims but are screened out due to credibility assessment errors, inconsistent adjudication standards across states, lack of legal representation, or documentary evidentiary demands that persecution flight rarely allows. They occupy the same legal category as recognized refugees in principle but bear the cost of a bifurcation the state administers imperfectly and unevenly.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, asylum_seekers_misclassified_at_screening, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__humanitarian_obligation_reading, asylum_seekers_misclassified_at_screening, beneficiary).

% Draft and administer the legal test distinguishing persecution-based claims from economic ones, staff the adjudication bureaucracy, and control detention, deportation, and appeal infrastructure. The categorical line lets them honor the humanitarian obligation narrowly while retaining broad discretion to exclude the much larger population of economically displaced people.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, destination_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from a bounded admission obligation that is politically defensible as morally motivated while remaining numerically limited. The persecution/economic distinction lets publics support humanitarian principle in the abstract without accepting the scale of admission that a needs-based or rights-based standard would require.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, host_state_publics, beneficiary,
    organized, generational, mobile, national).

% Administer refugee status determination, receive funding and mandate legitimacy from maintaining the persecution-based category as a workable, bounded legal instrument. Their institutional relevance depends on the category remaining administrable and distinct from the much larger and more diffuse economic migration question.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, unhcr_and_asylum_bureaucracies, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__humanitarian_obligation_reading, unhcr_and_asylum_bureaucracies, agenda_setter).

% Bear no formal role in the destination state's admission determination even where their own economic policy, governance failures, or climate adaptation choices contributed to the displacement. They are not party to the humanitarian-obligation framework's line-drawing, though remittance flows and diaspora relations make them structurally interested.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, origin_state_governments, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__humanitarian_obligation_reading, diffuse).
narrative_ontology:fixing_cost_class(border_legitimacy__humanitarian_obligation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally administrable, bounded standard letting states commit to a genuine humanitarian floor (non-refoulement for persecution victims) without the unbounded commitment a needs-based or rights-based admission standard would require — solving the real coordination problem of making a moral obligation operational and adjudicable at scale.
% TRANSFER_FUNCTION: Moves legal admission, protection status, and non-refoulement guarantees to those who can prove a persecution nexus, while withholding the same from those whose displacement is equally severe but categorized as economic or climate-driven — the transfer is legal standing itself, not a resource in the ordinary sense.
% ABSENT_VOICES: Economic migrants and climate-displaced persons have no forum in which to contest the categorical line itself; asylum tribunals adjudicate individual claims against the existing category, not the legitimacy of the category. Origin-state governments whose conditions produced the displacement are not parties to the admission determination at all.
% DISAPPEARANCE_RATIONALE: If the persecution/economic distinction vanished, either admission obligations would expand dramatically (moving toward the freedom-of-movement reading) or the humanitarian floor itself would collapse into pure sovereign discretion (moving toward the sovereignty reading) — either direction reorganizes global admission law, refugee bureaucracies, and the legal status of tens of millions of currently-excluded displaced people.
% FOUNDING_PROBLEM: Post-WWII architects of the 1951 Refugee Convention needed a way to obligate states to protect people fleeing state-sponsored persecution (particularly targeted political and ethnic violence) without requiring states to accept unlimited migration for any reason, which no state would ratify.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR and drafting-era diplomatic records attest the persecution-nexus problem was real and specific to Cold War-era political refugees. Independent migration scholars and origin-state governments increasingly attest that the founding problem has shifted: today's dominant displacement driver is climate and economic collapse, which the persecution framework was never built to address, making the categorical line's current function partly a legacy artifact rather than a live solution to the world's actual displacement pattern.
narrative_ontology:disappearance_verdict(border_legitimacy__humanitarian_obligation_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__humanitarian_obligation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__humanitarian_obligation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_legitimacy__humanitarian_obligation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__humanitarian_obligation_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__humanitarian_obligation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__humanitarian_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.52) because the categorical exclusion imposes a real, severe cost on economic migrants and climate-displaced persons who are denied a legal pathway that recognized refugees receive for comparably severe circumstances — but the exclusion is not extraction in the classic rent-seeking sense; no party directly profits from excluding economic migrants the way a landlord profits from rent. Suppression (0.58) reflects the active enforcement apparatus — detention, deportation, credibility-based denial — required to maintain the categorical line against migrants who would otherwise claim protection on humanitarian grounds. Theater ratio (0.42) is elevated because a substantial share of asylum adjudication activity increasingly functions to perform rigorous line-drawing (credibility interviews, country-conditions litigation) that adjudicates the boundary rather than serving the convention's original persecution-specific purpose, especially as economic and climate displacement increasingly present with persecution-adjacent framing to fit the available category. accessibility_collapse (0.5) and resistance (0.62) sit mid-range: alternatives to the categorical line (needs-based standards, temporary protected status expansions) exist and are actively contested by migration advocates, refugee-receiving publics, and origin-state governments — this is not a closed, uncontested arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the destination-state and bureaucratic seats, this reading is genuine coordination: a scalable, principled way to honor a real moral obligation without accepting unlimited liability. From the economic-migrant and climate-displaced seats, the identical legal apparatus operates as a wall dressed in humanitarian language — the persecution requirement does the work of exclusion while the framework's moral vocabulary (refugee protection, non-refoulement) supplies its legitimacy. The engine computes these as different seat-level types from the same structural data; neither seat's computed type is wrong, they are reading different positions in the same structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Recognized refugees and (nominally) misclassified asylum seekers are the coordination beneficiaries — the constraint's stated purpose is to protect them, and when classification succeeds it does. Economic migrants and climate-displaced persons are the structural targets: the same categorical apparatus that admits refugees is the apparatus that excludes them, with no separate mechanism bearing their case. Destination-state governments and host-state publics are beneficiaries in the directionality sense — they receive a bounded, politically sustainable humanitarian commitment rather than an open-ended one. UNHCR and asylum bureaucracies sit as institutional beneficiaries whose continued relevance depends on the persecution category remaining a workable administrative unit distinct from the much larger economic-migration question.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — obligating protection for Cold War-era political and ethnic persecution victims — was live and specific in 1951. Whether it remains the dominant global displacement problem today is contested: climate and economic collapse now drive a much larger share of forced displacement than state-targeted persecution, yet the legal category has not expanded to match. This is not classic mandatrophy (the founding problem is not simply dead) — persecution-based flight is still real and the obligation to protect it remains live — but the category's boundary has become mismatched to the world it was built to describe, which is why founding_problem_status is authored as contested rather than dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the humanitarian-obligation reading the correct framing of border legitimacy, or do the sovereignty_reading and freedom_of_movement_reading better capture the actual normative structure governing admission?',
    'No empirical resolution exists; this is a genuine normative dispute among live positions in political philosophy and international law, evidenced by the persistent three-way split among states, courts, and scholars (restrictionist sovereignty doctrine vs. expansive rights-based movement doctrine vs. the bounded humanitarian-obligation compromise embedded in the 1951 Convention and its regional analogues).',
    'If sovereignty_reading is correct, this constraint''s entire obligation structure is supererogatory rather than obligatory, collapsing much of its claimed victim set into non-victims of a mere policy choice. If freedom_of_movement_reading is correct, the persecution/economic distinction is itself the primary injustice, and economic_migrants and climate_displaced_without_persecution_nexus become the central victims of an illegitimate border regime rather than excluded parties to a bounded but legitimate one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which of the three border_legitimacy kernel readings correctly describes the normative structure of admission obligations.').

omega_variable(
    persecution_nexus_versus_severity_ambiguity,
    'Should admission obligation track the SOURCE of harm (persecution by an identifiable agent) or the SEVERITY of harm regardless of source (life-threatening displacement from any cause)?',
    'Comparative analysis of actual harm severity between recognized refugee populations and excluded climate/economic-displaced populations; if severity is comparable or greater for excluded groups, the persecution-nexus criterion is doing normative work disproportionate to its moral significance.',
    'If severity should govern, the current categorical line systematically misclassifies genuine humanitarian emergencies as non-qualifying, and the constraint''s moderate ε understates the harm to excluded groups. If source should govern (persecution carries distinct moral weight because of the identifiable perpetrator and rights violation, independent of comparative severity), the current line is more defensible than the severity comparison suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persecution_nexus_versus_severity_ambiguity, conceptual, 'Whether admission obligation should be triggered by harm source or harm severity.').

omega_variable(
    administrability_versus_justice_tradeoff,
    'Does the persecution/economic distinction''s administrative workability justify its under-inclusiveness, or does administrability provide cover for what is substantively an unjustifiable exclusion?',
    'Track whether alternative administrable standards (e.g., graduated protection categories, complementary protection regimes some states have adopted for climate displacement) achieve comparable adjudication cost and accuracy while including a broader population — if such standards exist and function, administrability does not require the current narrow line.',
    'If administrable alternatives exist and are not adopted, the persistence of the narrow persecution category is better explained by host-state political preference for bounded liability than by genuine administrative necessity, which would push this constraint''s classification toward snare rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrability_versus_justice_tradeoff, empirical, 'Whether the categorical line''s administrability is a genuine constraint or a post-hoc justification for a narrower obligation than administratively feasible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bord_tr_t8, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(bord_tr_t16, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(bord_tr_t24, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(bord_tr_t32, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(bord_tr_t40, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(bord_be_t8, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(bord_be_t16, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(bord_be_t24, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(bord_be_t32, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(bord_be_t40, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(bord_su_t8, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(bord_su_t16, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(bord_su_t24, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(bord_su_t32, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(bord_su_t40, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__humanitarian_obligation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__freedom_of_movement_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the border_legitimacy kernel. sovereignty_reading grounds border authority in territorial sovereignty with no independent admission obligation; freedom_of_movement_reading treats movement as a presumptive right against which any exclusion must be justified. This humanitarian_obligation_reading occupies the structural middle: it accepts legitimate state discretion to exclude (distinguishing it from freedom_of_movement_reading) while insisting a floor obligation exists that sovereignty alone cannot override (distinguishing it from sovereignty_reading). Each reading has its own ε, beneficiary/victim structure, and computed type; they are not to be merged or averaged. The expected structural delta for this reading, per the generation manifest, is the bifurcated victim set the persecution/economic line produces — a delta not present in either sibling reading's structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
