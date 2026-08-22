% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__endogenous_reinterpretation_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
 *   human_readable: Marriage Commitment Legitimacy: Endogenous Reinterpretation Reading
 *   domain: religious/institutional/political theology
 *
 * SUMMARY:
 *   This constraint describes the endogenous reinterpretation reading of the
 *   marriage commitment legitimacy kernel—specifically, the institutional
 *   narrative that the Manifesto (the documented reversal of prior marriage
 *   doctrine) represents genuine prophetic revelation and divine command to
 *   preserve the Church for higher purposes. Under this reading, federal
 *   legal pressure is the external context but NOT the cause of the doctrinal
 *   change. The institution authoritatively claims the reinterpretation as
 *   proof of ongoing divine guidance. The constraint is CLAIMED as
 *   tangled_rope (coordination via prophetic authority + extraction of
 *   doctrinal allegiance from those invested in the prior teaching) while the
 *   authored metrics show LOW extractiveness (0.28 at endpoint) and very LOW
 *   suppression (0.15), reflecting the reading's structural claim: the
 *   reinterpretation benefits divine authority and institutional continuity
 *   without requiring heavy coercive suppression because it redescribes
 *   continuity as evolutionary. This is a kernel reading; the competing
 *   readings (exogenous_override and hybrid_pragmatic) are separate
 *   constraints authored under their own sibling_ids.
 *
 * KEY AGENTS:
 *   - prophetic_authority_institution: Authoritatively declares and enforces the reinterpretation; identity-locked to the prophetic succession framework; benefits from demonstrated proof of ongoing revelation.
 *   - prior_doctrinal_commitment_holders: Adherents to the pre-Manifesto teaching; bear the cost of cognitive and credibility disruption; constrained exit (silence, departure, or reframing).
 *   - federal_authority_pressuring: External catalyst (exogenous legal pressure); structured out of the reinterpretation narrative as non-determinant; excluded from the institutional reinterpretation authority.
 *   - theological_continuity_preservers: The doctrinal framework itself benefits from the reinterpretation's demonstration that radical change can be accommodated within a continuous prophetic logic.
 *   - external_observers_and_critics: Historians, rival theologians, secular analysts who can document the federal pressure timeline and assess the reinterpretation's causal claims empirically.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.28).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.15).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "Marriage Commitment Legitimacy: Endogenous Reinterpretation Reading").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "religious/institutional/political theology").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'ccaf44d5-f466-47c9-a6d1-13a88ffe0232').
narrative_ontology:cs_kernel_codification('ccaf44d5-f466-47c9-a6d1-13a88ffe0232', formalized).
narrative_ontology:cs_authority_grounding('ccaf44d5-f466-47c9-a6d1-13a88ffe0232', lineage).
narrative_ontology:cs_interpretation_layer_present('ccaf44d5-f466-47c9-a6d1-13a88ffe0232').
narrative_ontology:cs_reading_relation('ccaf44d5-f466-47c9-a6d1-13a88ffe0232', marriage_commitment_legitimacy__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('ccaf44d5-f466-47c9-a6d1-13a88ffe0232', marriage_commitment_legitimacy__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('ccaf44d5-f466-47c9-a6d1-13a88ffe0232', foundational, manifesto_represents_genuine_divine_command).
narrative_ontology:cs_axiom_status(manifesto_represents_genuine_divine_command, holdable).
narrative_ontology:cs_axiom_grounding('ccaf44d5-f466-47c9-a6d1-13a88ffe0232', manifesto_represents_genuine_divine_command, theological).
narrative_ontology:cs_axiom('ccaf44d5-f466-47c9-a6d1-13a88ffe0232', foundational, prophetic_succession_legitimacy_requires_continuous_revelation).
narrative_ontology:cs_axiom_status(prophetic_succession_legitimacy_requires_continuous_revelation, holdable).
narrative_ontology:cs_axiom_grounding('ccaf44d5-f466-47c9-a6d1-13a88ffe0232', prophetic_succession_legitimacy_requires_continuous_revelation, deontological).
narrative_ontology:cs_reference_frame('ccaf44d5-f466-47c9-a6d1-13a88ffe0232', eternal_prophetic_marriage_doctrine).
narrative_ontology:cs_drift_state('ccaf44d5-f466-47c9-a6d1-13a88ffe0232', post_manifesto_reinterpretation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ccaf44d5-f466-47c9-a6d1-13a88ffe0232', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_authority_lineage).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, institutional_continuity_doctrine).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, pre_manifesto_doctrinal_commitment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prior_doctrinal_commitment_holders).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_command_framework).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_succession_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional bearer of prophetic succession authority (the Church leadership authoritatively interpreting and enforcing the new marriage doctrine derived from the Manifesto). Declares the Manifesto as genuine divine revelation and reinterprets marriage commitment as evolutionarily guided by God. Cannot exit this reading without losing the prophetic identity that constitutes institutional authority. Benefits from the reinterpretation because it proves ongoing divine guidance.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_authority_institution, agenda_setter,
    institutional, generational, identity_locked, global).

% Members and doctrinal adherents whose faith and credibility were invested in the pre-Manifesto teaching on marriage (eternal, unchangeable, divinely mandated). Bear the cost of doctrinal reversal: cognitive dissonance, loss of predictive authority in external debate ('you claimed this was eternal; now it is not'), alienation from those who reject the reframing. Can respond by silent reframing, departure, or argumentative defense of the new doctrine, but cannot reclaim the prior teaching's status without institutional reversal.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prior_doctrinal_commitment_holders, payer,
    organized, biographical, constrained, global).

% The exogenous legal and political force (federal government) applying pressure to abandon or modify marriage doctrine. Under this reading, federal pressure creates the crisis context but is NOT treated as causally determining the reinterpretation. The reading explicitly rejects the narrative that coercion produced the Manifesto, instead asserting divine command as the cause. Federal officials would dispute this reading (attesting they forced the change); their voices are excluded from the institutional authority that authoritatively declares the reinterpretation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, federal_authority_pressuring, excluded,
    institutional, generational, trapped, national).

% The doctrinal apparatus for maintaining theological coherence across institutional change (the interpretive methods and foundational concepts the Church uses to read scripture and doctrine). Benefits from the reinterpretation because it demonstrates that radical doctrinal reversals can be accommodated within a continuous prophetic framework—change becomes evolution, not rupture. The framework is vindicated by the reinterpretation's success in maintaining both doctrinal authority and institutional continuity.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theological_continuity_framework, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_non_agent(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theological_continuity_framework).

% Academic observers, historians, theologians from other traditions, and secular analysts who document the constraint from outside the institution's self-legitimating narrative. They have no stake in the prophetic authority's success and can assess whether the reinterpretation's claim to divine causation is empirically coherent with the federal pressure timeline and institutional internal sources. Their voices are excluded from the institutional authority but present in the historical and theological record.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, external_historians_and_theologians, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_authority_institution).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains institutional prophetic succession authority by demonstrating that God continues to guide the Church through doctrinal evolution. Solves the coordination problem: 'How does an institution claiming divine authority modify binding doctrine without losing legitimacy?' Answer: God commands the change, proved by the Manifesto. This reinterpretation coordinates the institution's members around a new theology while preserving the authority structure's claim to continuous revelation.
% TRANSFER_FUNCTION: Moves doctrinal allegiance from pre-Manifesto marriage commitment (eternal, unchangeable) to new commitment (monogamy as covenant stage, divorce permitted under conditions). Also moves credibility and cognitive resources from those invested in the prior teaching to the institution as the authoritatively proved source of new revelation. The payers are the prior commitment holders; the beneficiary is the prophetic authority institution.
% ABSENT_VOICES: Federal officials whose pressure occasioned the crisis would argue the Manifesto represents coerced capitulation, not divine revelation. Historical sources documenting federal legal timelines and pressure would dispute the reading's causal claim. Adherents to the prior doctrine who quietly left or were alienated by the reinterpretation are structurally absent from the institutional authority that declares the reinterpretation. Rival theological traditions that maintain the prior teaching (or reject the Manifesto altogether) are excluded from the institutional framing.
% DISAPPEARANCE_RATIONALE: If this reading disappeared (displaced by the exogenous_override or hybrid_pragmatic readings), the institution's narrative of the Manifesto would fundamentally shift: from 'God commanded this change' to 'we capitulated under pressure' or 'we strategically adapted while maintaining hidden theology.' The institution's claim to ongoing prophetic legitimacy would be severed or radically complicated. Institutional authority would appear coerced or merely pragmatic, not divinely guided. The theological continuity framework would lose its vindication—change would appear contingent, not evolutionary. Prior commitment holders would gain vindication (their resistance to the reinterpretation would appear justified). External observers and rival institutions would gain narrative dominance in the historical and theological record.
% FOUNDING_PROBLEM: How can an institution claiming eternal, unchanging prophetic doctrine respond when federal law contradicts that doctrine and exerts political pressure for institutional capitulation? The prior doctrine asserted marriage commitment as eternally binding, divinely mandated, unchangeable. Federal law moved toward permitting divorce. The institution faced a legitimacy crisis: maintain the prior doctrine and suffer legal penalties and institutional pressure, or modify doctrine and appear coerced. The reinterpretation solves this by reframing the doctrinal change as new divine command—God evolves the teaching to preserve the institution for higher purposes.
% FOUNDING_PROBLEM_CORROBORATION: The prophetic_authority_institution authoritatively attests the founding problem is solved and ongoing revelation is empirically proved. Federal authorities and historians attest the founding problem was exogenous coercion, not theological crisis—they document federal pressure timelines preceding the Manifesto and note the absence of pre-pressure internal theological debate initiating the change. Academic theology from outside the institution attests the reinterpretation is structurally defensive (theodicy work to preserve institutional authority under crisis) rather than independently grounded in textual evidence or pre-existing theological development. No corroborating source outside the prophetic authority institution attests the Manifesto represents genuine divine revelation as distinguished from strategic institutional reframing under federal pressure. The institution's own testimony is the sole internal witness to the claim.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).
:- end_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.28) because this reading's structural claim is that the reinterpretation is endogenous divine command, not exogenous coercion. Low extractiveness reflects the reading's own narrative: if God commanded the change, no coercive extraction is necessary; the institution is coordinating around new divine guidance. Suppression is very LOW (0.15) because the reading does not acknowledge federal pressure as causally determining—suppression in the structural sense (active coercion required to hold the constraint) is minimal under this framing. Theater_ratio is low-moderate (0.22) because while there is genuine theological work (reframing monogamy as a new covenant stage preserves doctrinal coherence), there is also a performative dimension (the reinterpretation must demonstrate that institutional authority is still prophetic). The measurement series shows slight rise in extractiveness and theater over the interval (as post-Manifesto doctrinal disputes accumulate and the institutional need to defend the reinterpretation grows), but suppression stays flat (the reading's structural claim does not require suppression to increase). This time grid is shared across all three metrics; every metric is authored at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The prophetic_authority_institution and the prior_doctrinal_commitment_holders should compute very different effective extraction (χ) from the same structural data. The institution sits as beneficiary and agenda_setter (d near 0.0 → low χ); the prior commitment holders sit as payers (d near 1.0 → higher χ). The external_observers_and_critics sit analytically (d = 0.5, no extraction). This divergence is expected and should be visible in the engine's per-seat classification output: the same constraint is experienced as prophetic coordination by the institution and as doctrinal extraction by the displaced adherents. The reading's power to define the situation (via prophetic authority) amplifies the divergence: the institution's framing dominates the public narrative, making the prior commitment holders' experience of extraction less visible.
 *
 * DIRECTIONALITY LOGIC:
 *   The prophetic_authority_institution is the primary beneficiary (collects the proof of ongoing revelation, which is its core identity-constituting resource; d near 0.0). Prior doctrinal commitment holders are the primary payers (bear doctrinal disruption, credibility cost, forced reframing; d near 1.0). Federal pressure is excluded (absent from the reinterpretation narrative, though causally present in context). The theological_continuity_preservers (doctrinal framework) benefits incidentally (the reinterpretation demonstrates the framework's flexibility; d near 0.2). External observers are analytical (d = 0.5). The institutional identity-lock (prophetic succession = what it means to be this institution) means the agenda_setter cannot exit even if the reinterpretation became costly—the reframing IS the institution's survival strategy in a crisis.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy would occur if the founding problem (how to maintain prophetic authority while reversing binding doctrine) became observably dead—i.e., if the institution abandoned the reinterpretation or if external pressure removed the occasion for the reinterpretation. Under this reading, the founding problem remains CONTESTED: the institution attests it is live and solved (God commanded the change); external observers attest the founding problem was federal coercion, not theological crisis. The mandatrophy test: if the Manifesto were repealed or reframed as coerced capitulation, the reinterpretation reading would collapse (the constraint would vanish). The fact that it has persisted for decades without institutional reversal suggests either the reading is functionally successful or the cost of abandoning it (admitting the prior doctrine was falsely claimed as eternal, admitting federal coercion determines doctrine) exceeds the cost of maintaining it. This is not mandatrophy yet; it is a live (contested) founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_command_empirical_grounding,
    'Is the Manifesto''s claim to represent divine command empirically distinguishable from institutional strategic reframing under federal pressure?',
    'Temporal sequence analysis: did doctrinal reinterpretation begin before, coincident with, or after federal pressure became acute? Textual analysis: does the Manifesto''s language reflect spontaneous revelation or strategic theology written to manage external crisis? Internal institutional sources: prior correspondence, theological debate, or dissent about marriage doctrine before federal pressure.',
    'If the Manifesto postdates federal pressure and shows rhetorical markers of crisis management, the reinterpretation is more likely endogenous strategic theology than exogenous divine command, shifting the reading toward exogenous_override or hybrid_pragmatic. If the reinterpretation preceded or independently developed from federal pressure, it supports the endogenous reading''s causal claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_command_empirical_grounding, empirical, 'Whether the reinterpretation''s timing and textual character support the claim to divine command vs. strategic reframing.').

omega_variable(
    prophetic_authority_identity_fusion,
    'Is the institution''s commitment to reading the Manifesto as divine command structurally necessary to maintain prophetic authority identity, or is it a chosen framing?',
    'Counterfactual institutional scenario: could the institution abandon the ''divine command'' reading and survive as a prophetic authority? Could it reframe the Manifesto as strategic adaptation while maintaining succession legitimacy? Historical comparison: do other prophetic institutions maintain authority through doctrinal change without claiming each change as new revelation?',
    'If the prophetic authority identity is structurally dependent on the ''divine command'' reading (cannot be abandoned without institutional dissolution), the identity_locked exit classification is validated and the reinterpretation becomes non-negotiable—extraction becomes existential defense. If alternative identity framings are possible, the identity_lock is weaker and the reinterpretation is more subject to negotiated revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prophetic_authority_identity_fusion, conceptual, 'Whether prophetic authority legitimacy requires the ''divine command'' reading of the Manifesto or whether alternative institutional narratives could sustain succession authority.').

omega_variable(
    federal_pressure_causal_determination,
    'Under this reading, federal pressure is treated as external occasion, not determinant. Is this structural distinction defensible, or does the pressure''s timing and intensity make it the primary cause of the reinterpretation?',
    'Counterfactual: would the institution have reinterpreted marriage doctrine absent federal pressure? Mechanism analysis: can the institution articulate internal theological reasons for the reinterpretation that would have driven change independently? Institutional testimony: did the institution plan or debate this change before federal pressure, or only after?',
    'If federal pressure was necessary and sufficient to trigger the reinterpretation (had pressure not occurred, no change would have happened), the ''external occasion'' claim is false and the causal structure shifts toward exogenous_override. If internal theological development would have driven the change eventually (federal pressure merely accelerated it), the occasion/determination distinction holds and the endogenous reading is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_pressure_causal_determination, empirical, 'Whether federal pressure is structurally separable from the causal chain producing the reinterpretation, or whether pressure is the primary determinant.').

omega_variable(
    sibling_reading_foreclosure_or_coexistence,
    'Do the three readings (endogenous, exogenous, hybrid) logically foreclose one another, or do they coexist as live options held by different institutional and analytical seats?',
    'Logical analysis: are the core causal claims mutually exclusive (only one can be true)? Institutional sociology: which reading is authoritatively endorsed by the institution, and which are held by dissenting factions, historical scholars, or external observers? Pragmatic stakes: would accepting one reading require rejecting another, or can the institution hold the endogenous reading while acknowledging that external observers hold different readings?',
    'If the readings logically foreclose one another (only one causal story is true about the Manifesto), they should be marked as forecloses in reading_relations. If they coexist as different parties'' live narratives despite logical incompatibility, they are coexists_with. If they form a hierarchy (one reading provides scaffolding for the others), they influence one another.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_or_coexistence, conceptual, 'The logical and institutional structure relating the three sibling readings of the marriage_commitment_legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(marr_tr_t5, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t15, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement_basis(marr_tr_t15, observed).
narrative_ontology:measurement(marr_tr_t25, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 25, 0.21).
narrative_ontology:measurement_basis(marr_tr_t25, observed).
narrative_ontology:measurement(marr_tr_t40, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(marr_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement_basis(marr_be_t5, observed).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t15, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 15, 0.27).
narrative_ontology:measurement_basis(marr_be_t15, observed).
narrative_ontology:measurement(marr_be_t25, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement_basis(marr_be_t25, observed).
narrative_ontology:measurement(marr_be_t40, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(marr_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 5, 0.13).
narrative_ontology:measurement_basis(marr_su_t5, observed).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t15, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement_basis(marr_su_t15, observed).
narrative_ontology:measurement(marr_su_t25, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement_basis(marr_su_t25, observed).
narrative_ontology:measurement(marr_su_t40, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement_basis(marr_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.12).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% The marriage_commitment_legitimacy kernel decomposes into three constraint stories, one per reading: endogenous_reinterpretation (divine command drove the change), exogenous_override (federal coercion forced capitulation), and hybrid_pragmatic (strategic institutional deployment managing exogenous crisis). Each reading has a different ε because the referent (the Manifesto's legitimacy) is evaluated by each reading's own lights—what counts as extraction depends on the causal story. These are not measurements of the same constraint from different angles; they are different constraints instantiated by different readings of the contested kernel. Link via network.affects_constraints to establish the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
