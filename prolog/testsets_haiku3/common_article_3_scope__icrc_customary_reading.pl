% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: CA3 Scope Determination via Customary International Law Evolution
 *   domain: international/humanitarian_law
 *
 * SUMMARY:
 *   Common Article 3 (CA3) of the Geneva Conventions sets minimum
 *   humanitarian standards for armed conflicts not of an international
 *   character. The treaty text uses implicit thresholds: conflicts must reach
 *   a certain intensity and organization level to trigger CA3 minimums. By
 *   the 1970s–1980s, states increasingly applied CA3 to lower-intensity armed
 *   violence. The International Committee of the Red Cross (ICRC) developed
 *   an institutional reading that CA3 scope is determined by evolving state
 *   practice and opinio juris, tracked through customary international law.
 *   This reading creates a procedural mechanism for scope expansion without
 *   formal treaty amendment. It also vests interpretive authority in the ICRC
 *   to assess whether specific conflicts meet the threshold. The constraint
 *   is ICRC_CUSTOMARY_READING: a particular institutional interpretation of
 *   how CA3 scope evolves. It competes with two sibling readings: the
 *   state_centric_reading (bright-line intensity thresholds, conservative
 *   scope) and the expansive_human_rights_reading (CA3 as a universal floor
 *   to any organized violence). This story instantiates ONLY the ICRC reading
 *   as a clean constraint—an institutional coordination mechanism that also
 *   creates asymmetric control over humanitarian protections.
 *
 * KEY AGENTS:
 *   - International Committee of the Red Cross (ICRC): institutional gatekeeper of customary-law scope determination; sets the procedural authority structure
 *   - States with flexible application preferences: benefit from discretionary threshold interpretation while retaining formal compliance posture
 *   - Armed groups and non-state combatants: bound by CA3 minimums when ICRC determines their conflict meets the opinio juris threshold, but lack direct voice in that determination
 *   - Civilian populations in low-intensity conflict zones: protected by CA3 when ICRC recognizes the threshold is met, vulnerable when the recognition lags violence escalation
 *   - State-centric reading adherents (typically powerful states preferring bright-line rules): excluded from the procedural consensus; their preference for legal certainty is systematized as non-cooperative
 *   - Expansive human-rights reading advocates (NGOs, human rights bodies): excluded from opinio juris determination; their position that CA3 should be a universal baseline is marginalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.58).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.42).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "CA3 Scope Determination via Customary International Law Evolution").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "international/humanitarian_law").

domain_priors:requires_active_enforcement(common_article_3_scope__icrc_customary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, '25034251-8ed5-4518-9980-9792d3f6863d').
narrative_ontology:cs_kernel_codification('25034251-8ed5-4518-9980-9792d3f6863d', distributed).
narrative_ontology:cs_authority_grounding('25034251-8ed5-4518-9980-9792d3f6863d', lineage).
narrative_ontology:cs_interpretation_layer_present('25034251-8ed5-4518-9980-9792d3f6863d').
narrative_ontology:cs_reading_relation('25034251-8ed5-4518-9980-9792d3f6863d', common_article_3_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('25034251-8ed5-4518-9980-9792d3f6863d', common_article_3_scope__expansive_human_rights_reading, influences).
narrative_ontology:cs_axiom('25034251-8ed5-4518-9980-9792d3f6863d', foundational, opinio_juris_as_valid_scope_mechanism).
narrative_ontology:cs_axiom_status(opinio_juris_as_valid_scope_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('25034251-8ed5-4518-9980-9792d3f6863d', opinio_juris_as_valid_scope_mechanism, conventional).
narrative_ontology:cs_axiom('25034251-8ed5-4518-9980-9792d3f6863d', secondary, icrc_institutional_interpretation_authority).
narrative_ontology:cs_axiom_status(icrc_institutional_interpretation_authority, holdable).
narrative_ontology:cs_axiom_grounding('25034251-8ed5-4518-9980-9792d3f6863d', icrc_institutional_interpretation_authority, conventional).
narrative_ontology:cs_reference_frame('25034251-8ed5-4518-9980-9792d3f6863d', customary_law_scope_determination).
narrative_ontology:cs_drift_state('25034251-8ed5-4518-9980-9792d3f6863d', contemporary_humanitarian_operations_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('25034251-8ed5-4518-9980-9792d3f6863d', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, icrc_and_humanitarian_doctrine).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, states_with_flexible_application).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, armed_groups_nonstate_combatants).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, civilian_populations_in_contested_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, international_committee_red_cross).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, humanitarian_access_negotiators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and documents customary international law on CA3 scope through field reports, legal commentary, and institutional practice guidance. Maintains the reading that CA3 scope evolves through demonstrated state practice and opinio juris, allowing scope expansion without formal treaty amendment. Benefits from the procedural flexibility this reading provides for humanitarian access and negotiation. Sets the authoritative interpretation architecture for humanitarian actors.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, international_committee_red_cross, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, international_committee_red_cross, beneficiary).

% States that benefit from the customary-law framing because it allows them to apply CA3 selectively based on opinio juris judgments. Can claim compliance while retaining discretion over when a conflict reaches the threshold for CA3 application. Avoids formal treaty renegotiation that would lock in obligations. Reserves the right to contest the ICRC's reading of state practice.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, states_with_flexible_application, beneficiary,
    powerful, generational, mobile, global).

% Bound by CA3 minimums when ICRC determination (via customary-law logic) deems a conflict sufficiently organized. But the opinio juris test leaves scope ambiguous at the margins: whether their conflict qualifies depends on evolving state practice interpretations they do not control. Trapped by the procedural gate that governs their status.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, armed_groups_nonstate_combatants, payer,
    organized, biographical, identity_locked, local).

% Protection under CA3 depends on whether their armed context meets the opinio juris threshold at that moment. The customary-law framing means CA3 coverage can lag actual violence: a conflict zone may operate below the recognized threshold for months or years, leaving civilians outside the formal humanitarian umbrella, even as violence escalates.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, civilian_populations_in_contested_zones, payer,
    powerless, immediate, trapped, local).

% States preferring narrow, bright-line intensity thresholds for CA3 applicability are systematically excluded from the procedural consensus the customary-law reading constructs. Their preference for legal certainty and state sovereignty is treated as non-cooperative. They remain in the formal system but their interpretations are marginalized.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, state_centric_reading_adherents, excluded,
    powerful, generational, constrained, global).

% Human rights organizations and advocates pressing for CA3 to apply as a universal floor to any organized violence are excluded from the procedural authority structure the ICRC reading establishes. Their position that CA3 should be a baseline, not a threshold-dependent coordinate, does not shape the opinio juris determination.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, expansive_human_rights_reading_advocates, excluded,
    organized, generational, constrained, global).

% Benefit from the customary-law framing because the evolving-threshold logic creates a negotiation space: arguing that state practice has shifted to recognize a lower intensity threshold can unlock ICRC access and CA3 minimum guarantees. The procedural flexibility enables tactical humanitarian concessions.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, humanitarian_access_negotiators, beneficiary,
    organized, biographical, constrained, global).

% Analytical seat: documents, critiques, and influences the scholarly consensus about what state practice and opinio juris actually represent. Their work shapes which readings are considered defensible within the broader legal academy.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, legal_scholars_and_doctrine, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__icrc_customary_reading, international_committee_red_cross).
narrative_ontology:fixing_cost_class(common_article_3_scope__icrc_customary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables incremental harmonization of humanitarian standards across armed conflicts of varying intensity without requiring formal treaty amendment. Allows the ICRC to extend protections as consensus shifts and states adjust practice, avoiding deadlock in formal renegotiation.
% TRANSFER_FUNCTION: Transfers interpretive authority from explicit legal text to evolving state practice and opinio juris, determined primarily through ICRC documentation and institutional judgment. Moves the locus of scope definition from formal treaty processes to customary-law tracking, which is controlled procedurally by humanitarian institutions.
% ABSENT_VOICES: States preferring certainty and bright-line thresholds (state_centric_reading) and human rights advocates pressing for a universal floor (expansive_human_rights_reading) are excluded from the opinio juris determination process. They can contest readings but do not set the procedural framework.
% DISAPPEARANCE_RATIONALE: If the customary-law scope determination vanished, CA3 would revert to textual boundaries (intensity and organization thresholds from the treaty) or states would have to renegotiate formally. The procedural coordination mechanism that allows gradual scope expansion would collapse; either formal deadlock would result or CA3 coverage would narrow to explicit textual consensus.
% FOUNDING_PROBLEM: Common Article 3 text was written in 1949 with implicit thresholds for conflict intensity and organization. By the 1980s, state practice had evolved to recognize lower-intensity armed violence as triggering CA3 minimums. Formal amendment was blocked; customary international law became the mechanism for scope evolution without renegotiation.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC Study on Customary International Law (2005) documents this evolution across state practice. The International Court of Justice has cited customary law as the basis for CA3 scope expansion (Nicaragua case, Tadic precedent). Human rights treaty bodies and IHL scholars outside the beneficiary set also affirm that state practice has shifted.
narrative_ontology:disappearance_verdict(common_article_3_scope__icrc_customary_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__icrc_customary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__icrc_customary_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 at interval end) because the ICRC's interpretive authority over opinio juris allows it to effectively gate humanitarian protections without formal legal amendment. States retain nominal sovereignty over opinio juris consensus, but the ICRC's institutional position in field operations and legal documentation gives it substantial procedural control. The measurement series shows rising extractiveness from 1949 (0.35) to 2005 (0.58): as state practice actually diverged from the treaty text and the customary-law framing became necessary, the ICRC's authority to interpret that divergence and adjudicate CA3 applicability increased. Theater_ratio rises from 0.08 to 0.31: early in the interval, the ICRC's scope interpretation tracked genuine state practice; over time, an increasing portion of the ICRC's determination activity went to documenting and justifying the scope expansion narratively rather than discovering new state consensus—the constraint became more about maintaining the interpretation architecture than discovering facts. Suppression is moderate (0.42) because the customary-law reading operates through consensus-building and doctrine development, not brute force; however, it still requires suppressing the state_centric and expansive_human_rights_readings to maintain procedural authority. Accessibility_collapse at 0.65 reflects that once the customary-law framing is accepted as the locus of scope determination, the alternatives (formal amendment, universal baseline, bright-line rules) recede—the procedural lock-in is substantial. Resistance at 0.72 reflects real pushback from states preferring certainty and human rights advocates pressing for expansion; the constraint is actively contested, not passively inherited.
 *
 * PERSPECTIVAL GAP:
 *   From the ICRC's seat, the constraint is genuine coordination: it enables humanitarian protections to expand as state practice shifts, avoiding deadlock. From a powerful state's seat, it is flexible discretion: they can apply CA3 selectively while claiming consensus. From a nonstate combatant's or civilian's seat, it is opaque procedural gatekeeping: their legal status depends on institutional determinations they cannot contest or predict. The engine should compute these as divergent types—rope-like coordination from the ICRC perspective, tangled-rope extraction from the payer seats. The authored metrics reflect the lowest-power perspectives (civilian protection, combatant status ambiguity) and thus favor the extraction reading; the claim (tangled_rope) matches this payer-seat perspective rather than the ICRC's own coordination narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICRC sits at near-full beneficiary (d ≈ 0.15): it benefits from institutional authority over scope determination, formalizes humanitarian doctrine, and maintains interpretive control without bearing extraction costs. Flexible-application states are partial beneficiaries (d ≈ 0.25): they gain discretion over CA3 applicability while avoiding formal amendment deadlock, but they also accept the ICRC as procedural gatekeeper. Armed groups and nonstate combatants are near full targets (d ≈ 0.85): they are bound by CA3 minimums determined by institutions they do not control, and their combatant status depends on ICRC threshold judgments. Civilian populations are also targets (d ≈ 0.90): their protection is contingent on institutional scope determinations they cannot influence, and delays in recognition leave them unprotected. Excluded readings (state-centric, expansive human-rights) are targets of institutional suppression (d ≈ 0.75): their positions are systematically marginalized from the opinio juris procedure. The directionality derivation does not require overrides because the structural data (beneficiary/victim/exit) maps cleanly to the power atoms and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state practice exceeded 1949 treaty text; formal amendment was blocked) is clearly live: CA3 applicability still tracks actual armed violence below the textual thresholds. But there is a secondary mandatrophy signal: as doctrine and precedent have solidified, the ICRC's opinio juris determinations have become increasingly formalized and less contestable—the determination method, not the discovery of state practice, drives outcomes. If the constraint resolved and CA3 scope were renegotiated explicitly or abandoned, the result would likely be either formal bright-line thresholds (state_centric_reading) or a statutory universal floor (expansive_human_rights_reading). The customary-law reading persists partly because it avoids having to choose between those alternatives; institutional momentum maintains it even as the original problem it solved becomes more proceduralized than substantive. This is not yet full mandatrophy (the constraint still serves a function), but it is drift toward one. The theater_ratio's steady climb (0.08 to 0.31) is the temporal signal of this drift: documentation and justification activity is increasing faster than genuine new scope expansion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opinio_juris_measurement_ambiguity,
    'What constitutes the ''opinio juris'' that determines CA3 scope? Is it formal state declarations, actual practice on the ground, ICRC determinations, or consensus among legal doctrine?',
    'Comparative analysis of state practice vs. formal statements; scrutiny of ICRC methodology for tracking opinio juris; examination of whether a conflict meets the ICRC''s threshold at the moment of determination.',
    'If opinio juris is ICRC-determined from practice interpretation, the constraint is extractive and institutional; if it is formally declared by states, it is more distributed. This affects both the power imbalance and the theater ratio—the current measurement assumes ICRC interpretation gatekeeps, but that claim is contestable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(opinio_juris_measurement_ambiguity, conceptual, 'The referent of ''opinio juris'' in practice: who measures it, by what standard, and how binding are their determinations?').

omega_variable(
    institutional_authority_vs_state_sovereignty,
    'Does the customary-law reading actually coordinate humanitarian standards through state practice, or does it vest authority in the ICRC to determine scope on humanitarian grounds, reducing states to post-hoc ratification?',
    'Historical analysis of conflicts where ICRC made scope determinations states later accepted or resisted; interviews with state delegates and ICRC staff on the mechanics of opinio juris determination; examination of whether states genuinely participate in threshold-setting or only formalize ICRC judgments.',
    'If the ICRC''s determination is binding in practice while formally subject to state consensus, the constraint''s extraction is higher (the ICRC extracts procedural authority) and the suppression is lower (states go along because the humanitarian need is real, not because they are coerced—making this a false summit candidate rather than a tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_vs_state_sovereignty, empirical, 'Whether the customary-law procedure genuinely coordinates state practice or is an institutional authority wrapped in state-practice language.').

omega_variable(
    reading_foreclosure_ambiguity,
    'Does the ICRC customary-law reading foreclose the state_centric_reading (bright-line thresholds), or do they coexist with different states holding different interpretations?',
    'Examination of state positions in international forums, treaty negotiations, and court proceedings; survey of whether any major states formally endorse state_centric_reading while accepting the ICRC customary-law framing in practice.',
    'If foreclosure is occurring, the reading should be marked `forecloses` rather than `coexists_with`; if coexistence is the actual structure (different states use different frameworks), the reading''s extractiveness is higher because the ICRC''s framing is contested and maintained through institutional leverage rather than consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_ambiguity, empirical, 'Whether the ICRC reading eliminates or merely marginalizes the competing state-centric interpretation.').

omega_variable(
    threshold_drift_vs_interpretation_drift,
    'Over the interval 1949–2025, did CA3''s scope genuinely expand (states practice CA3 to lower-intensity conflicts), or did interpretation become more permissive while practice remained constant?',
    'Archival analysis of state practice in specific conflicts (intensity, organization, whether CA3 was applied) at multiple time points; comparison of ICRC field judgments across eras; examination of whether modern low-intensity conflict recognition is driven by actual state behavior or by interpretive doctrinal shift.',
    'If practice expanded, the customary-law reading genuinely coordinates around a shifting consensus; if interpretation expanded while practice remained stable, the constraint is more extractive (institutions are reshaping doctrine without state practice support) and the theater_ratio should be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_drift_vs_interpretation_drift, empirical, 'Whether the measurement series reflects actual scope change or hermeneutic drift.').

omega_variable(
    procedural_vs_substantive_scope,
    'Is this constraint a procedural mechanism for scope determination (a metarule about how to interpret CA3), or is it itself a substantive constraint on CA3''s application?',
    'Examination of whether the ICRC''s methodology and findings are replicable and transparent (procedural markers) or depend on institutional judgment and field authority (substantive markers); analysis of whether the constraint would vanish if the procedural method were made explicit or if it requires ongoing ICRC judgment.',
    'If substantive, the constraint is closer to a snare (extraction via scope gatekeeping); if procedural, it is closer to a rope with institutional coordination overhead. The current claimed_type is tangled_rope; this omega tests whether the procedural framing is genuine or a cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_vs_substantive_scope, conceptual, 'Whether the customary-law reading is a rule for interpretation or a doctrine that reshapes CA3 itself.').

omega_variable(
    kernel_reading_identity_fusion,
    'Is this reading held by the ICRC as a coherent institutional commitment, or is it an evolved doctrine that the ICRC opportunistically invokes when beneficial and abandons when inconvenient?',
    'Examination of ICRC consistency across field operations and legal publications; archival analysis of moments where the ICRC either defended or departed from the customary-law framing; interviews with ICRC legal staff on whether the reading is a foundational principle or a tactical tool.',
    'If the reading is coherent and foundational, the constraint is sustained by genuine institutional commitment. If opportunistic, the extraction is higher because the ICRC reserves the right to unilaterally reinterpret CA3 scope as circumstances shift—making the constraint closer to a snare (institutional gatekeeping with minimal state input) than a tangled rope (hybrid coordination with real benefit-sharing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity_fusion, empirical, 'Whether the ICRC''s customary-law reading is an identity-fused institutional commitment or a flexible tactical doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__icrc_customary_reading, theater_ratio, 1949, 0.08).
narrative_ontology:measurement_basis(comm_tr_t1949, observed).
narrative_ontology:measurement(comm_tr_t1977, common_article_3_scope__icrc_customary_reading, theater_ratio, 1977, 0.15).
narrative_ontology:measurement_basis(comm_tr_t1977, observed).
narrative_ontology:measurement(comm_tr_t1995, common_article_3_scope__icrc_customary_reading, theater_ratio, 1995, 0.24).
narrative_ontology:measurement_basis(comm_tr_t1995, observed).
narrative_ontology:measurement(comm_tr_t2005, common_article_3_scope__icrc_customary_reading, theater_ratio, 2005, 0.29).
narrative_ontology:measurement_basis(comm_tr_t2005, observed).
narrative_ontology:measurement(comm_tr_t2015, common_article_3_scope__icrc_customary_reading, theater_ratio, 2015, 0.31).
narrative_ontology:measurement_basis(comm_tr_t2015, observed).
narrative_ontology:measurement(comm_tr_t2025, common_article_3_scope__icrc_customary_reading, theater_ratio, 2025, 0.31).
narrative_ontology:measurement_basis(comm_tr_t2025, projected).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1949, 0.35).
narrative_ontology:measurement_basis(comm_be_t1949, observed).
narrative_ontology:measurement(comm_be_t1977, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1977, 0.48).
narrative_ontology:measurement_basis(comm_be_t1977, observed).
narrative_ontology:measurement(comm_be_t1995, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1995, 0.54).
narrative_ontology:measurement_basis(comm_be_t1995, observed).
narrative_ontology:measurement(comm_be_t2005, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement_basis(comm_be_t2005, observed).
narrative_ontology:measurement(comm_be_t2015, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement_basis(comm_be_t2015, observed).
narrative_ontology:measurement(comm_be_t2025, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement_basis(comm_be_t2025, projected).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1949, 0.25).
narrative_ontology:measurement_basis(comm_su_t1949, observed).
narrative_ontology:measurement(comm_su_t1977, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1977, 0.32).
narrative_ontology:measurement_basis(comm_su_t1977, observed).
narrative_ontology:measurement(comm_su_t1995, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement_basis(comm_su_t1995, observed).
narrative_ontology:measurement(comm_su_t2005, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2005, 0.41).
narrative_ontology:measurement_basis(comm_su_t2005, observed).
narrative_ontology:measurement(comm_su_t2015, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement_basis(comm_su_t2015, observed).
narrative_ontology:measurement(comm_su_t2025, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2025, 0.42).
narrative_ontology:measurement_basis(comm_su_t2025, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_article_3_scope__icrc_customary_reading, 0.12).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, principle_of_distinction_application).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, proportionality_assessment_in_armed_conflict).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the common_article_3_scope kernel. The ICRC customary-law reading frames CA3 scope as procedurally determined through opinio juris tracking. The state_centric_reading frames it as textually fixed by intensity thresholds. The expansive_human_rights_reading frames it as a universal baseline. Each reading has distinct ε, beneficiary/victim structure, and extracted asymmetries. They do not compete for dominance in a single framework; rather, different institutional actors (ICRC, state governments, human rights bodies) hold different readings and interpret CA3 accordingly. The network links capture how shifts in one reading's institutional authority affect the others' leverage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
