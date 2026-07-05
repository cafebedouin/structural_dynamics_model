% ============================================================================
% CONSTRAINT STORY: naskh_principle__progressive_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__progressive_restriction, []).

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
 *   constraint_id: naskh_principle__progressive_restriction
 *   human_readable: Progressive Restriction (Tadarruj) Reading of Naskh
 *   domain: Islamic Jurisprudence / Quranic Hermeneutics / Legal Theory
 *
 * SUMMARY:
 *   This story instantiates the progressive_restriction reading of the naskh
 *   (abrogation) kernel: rather than treating later Quranic verses as
 *   formally abrogating earlier ones on the same topic
 *   (classical_abrogation), or treating all verses as independently valid
 *   within their original context (contextual_harmonization), this reading
 *   holds that the sequence from permissive to restrictive verses on a given
 *   topic constitutes a single graduated pedagogical process — divine
 *   accommodation of human capacity moving toward a final, more restrictive
 *   intended norm. Structurally, this benefits scholars and institutions
 *   positioned to narrate the pedagogical arc (a form of interpretive
 *   authority not reducible to citing a fixed abrogation-pair or a contextual
 *   specification) and disadvantages those who cite early permissive verses
 *   for present practice, since their citation is recast as reliance on a
 *   superseded transitional stage rather than a permanently valid or
 *   contextually bounded ruling. The three readings are NOT versions of one
 *   constraint measured differently — each has a distinct beneficiary/victim
 *   structure and a distinct ε; they are linked stories in a kernel family,
 *   not one story with a parameter.
 *
 * KEY AGENTS:
 *   - evolutionary_legal_scholars: agenda_setter/beneficiary (institutional/arbitrage) — administers the pedagogical-arc doctrine
 *   - pedagogical_authority_institutions: beneficiary/agenda_setter (institutional/arbitrage) — certifies which reading is taught
 *   - practitioners_citing_early_permissive_verses: payer (moderate/constrained) — loses textual footing for contemporary practice
 *   - communities_relying_on_wine_and_intoxicant_leniency_texts: payer (powerless/trapped) — bears direct social/legal cost
 *   - classical_abrogation_scholars: excluded (organized/constrained) — sidelined rival methodology, not silenced
 *   - comparative_religious_historians: observer (analytical/analytical) — traces institutional emergence of all three readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.52).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.58).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.52).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, tangled_rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Progressive Restriction (Tadarruj) Reading of Naskh").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "Islamic Jurisprudence / Quranic Hermeneutics / Legal Theory").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, '7f377d1c-eb81-45de-820e-3c686a1871cf').
narrative_ontology:cs_kernel_codification('7f377d1c-eb81-45de-820e-3c686a1871cf', fixed_text).
narrative_ontology:cs_authority_grounding('7f377d1c-eb81-45de-820e-3c686a1871cf', lineage).
narrative_ontology:cs_interpretation_layer_present('7f377d1c-eb81-45de-820e-3c686a1871cf').
narrative_ontology:cs_reading_relation('7f377d1c-eb81-45de-820e-3c686a1871cf', naskh_principle__classical_abrogation, coexists_with).
narrative_ontology:cs_reading_relation('7f377d1c-eb81-45de-820e-3c686a1871cf', naskh_principle__contextual_harmonization, influences).
narrative_ontology:cs_axiom('7f377d1c-eb81-45de-820e-3c686a1871cf', foundational, revelatory_sequence_carries_pedagogical_intent).
narrative_ontology:cs_axiom_status(revelatory_sequence_carries_pedagogical_intent, holdable).
narrative_ontology:cs_axiom_grounding('7f377d1c-eb81-45de-820e-3c686a1871cf', revelatory_sequence_carries_pedagogical_intent, deontological).
narrative_ontology:cs_axiom('7f377d1c-eb81-45de-820e-3c686a1871cf', secondary, later_restriction_reflects_final_not_superseding_intent).
narrative_ontology:cs_axiom_status(later_restriction_reflects_final_not_superseding_intent, holdable).
narrative_ontology:cs_axiom_grounding('7f377d1c-eb81-45de-820e-3c686a1871cf', later_restriction_reflects_final_not_superseding_intent, conventional).
narrative_ontology:cs_reference_frame('7f377d1c-eb81-45de-820e-3c686a1871cf', classical_gradualist_pedagogy_framework).
narrative_ontology:cs_drift_state('7f377d1c-eb81-45de-820e-3c686a1871cf', contemporary_reformist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7f377d1c-eb81-45de-820e-3c686a1871cf', '').
narrative_ontology:cs_kernel_id(naskh_principle__progressive_restriction, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, evolutionary_legal_scholars).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, reform_oriented_jurists).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, pedagogical_authority_institutions).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, practitioners_citing_early_permissive_verses).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, literalist_minority_schools).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, communities_relying_on_wine_and_intoxicant_leniency_texts).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, divine_pedagogy_doctrine).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, gradualism_in_revelation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author and teach the tadarruj (gradualism) framework, positioning the arc from permissive to restrictive verses as a single coherent pedagogical trajectory rather than sets of independent or abrogating rulings. This framing lets them argue for continued legal evolution beyond the classical corpus by appeal to the same 'divine pedagogy' logic, and they administer seminary curricula and fatwa councils that certify which reading is taught as authoritative.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, evolutionary_legal_scholars, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__progressive_restriction, evolutionary_legal_scholars, beneficiary).

% Draw on the progressive-restriction model to argue that revelation itself models an arc toward increasing restriction (or, selectively, toward loosening) that later interpreters may continue reading contextually. They gain interpretive latitude from a framework that treats the textual sequence as instructive process rather than fixed abrogation-pairs, but depend on institutional recognition to have their readings taken seriously.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, reform_oriented_jurists, beneficiary,
    organized, generational, mobile, national).

% Seminaries, fiqh academies, and state-linked religious authorities that certify the progressive-restriction reading as orthodox pedagogy. They benefit from a framework that casts their interpretive mediation as necessary — someone must explain what stage of 'divine pedagogy' applies to a modern question — which entrenches their gatekeeping role over legal derivation.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, pedagogical_authority_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__progressive_restriction, pedagogical_authority_institutions, agenda_setter).

% Individuals or minority communities who invoke earlier, more permissive Quranic verses (on inheritance shares, testimony, marriage practice, or intoxicant tolerance) to justify contemporary practice. Under this reading their citation is treated as reliance on a superseded 'transitional accommodation,' not a permanently valid ruling, which delegitimizes their practice without a formal abrogation finding they could contest on textual grounds.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, practitioners_citing_early_permissive_verses, payer,
    moderate, biographical, constrained, national).

% Small schools or sects that hold every verse individually binding regardless of revelatory sequence. The progressive-restriction reading structurally displaces their entire hermeneutic, since it treats sequence itself as carrying binding pedagogical weight — a premise their tradition rejects. They have no institutional platform from which to contest the dominant seminary consensus.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, literalist_minority_schools, payer,
    powerless, generational, trapped, regional).

% Communities or individuals whose local practice draws on earlier, more lenient Quranic passages regarding intoxicants, treated under this reading as an early pedagogical waypoint superseded by later restriction. They bear direct social and legal costs (censure, exclusion, prosecution under religious-law-influenced statutes) for practices the progressive-restriction frame declares definitively closed rather than merely one contextual reading among others.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, communities_relying_on_wine_and_intoxicant_leniency_texts, payer,
    powerless, biographical, trapped, local).

% Hold that later verses formally abrogate (nasikh) earlier ones (mansukh) as discrete textual events, verse-pair by verse-pair, rather than as stages in a continuous pedagogical arc. Their classical abrogation-catalog methodology is sidelined when a seminary or fatwa council instead teaches the progressive-restriction narrative, though they remain a live competing school rather than a silenced one.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, classical_abrogation_scholars, excluded,
    organized, civilizational, constrained, global).

% Hold that all verses remain independently valid within their original situational context, with no verse superseding another. This reading is structurally at odds with treating the sequence as directional divine pedagogy; where progressive-restriction becomes the taught doctrine, harmonization scholars' contextual-specification method is marginalized in curricula and fatwa practice, though they continue to publish and teach elsewhere.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, contextual_harmonization_scholars, excluded,
    organized, civilizational, constrained, global).

% Study how the three naskh readings emerged historically, whom each empowers, and how legal-theological consensus shifted across centuries and regions. They can trace which reading dominates in which institutional context and why, without being party to any of the three traditions' internal legitimacy claims.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, comparative_religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single coherent narrative for reconciling apparently contradictory Quranic rulings on the same topic (e.g., intoxicants, testimony, marriage) by casting the sequence of revelation itself as a graduated pedagogical process, allowing jurists to derive one settled ruling per topic instead of leaving multiple textually 'valid' but contradictory rulings unresolved.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy from those who cite earlier permissive verses (or from rival abrogation/harmonization schools) to institutions and scholars positioned to authoritatively narrate 'where a topic sits' in the pedagogical arc — shifting practical legal deference, and in jurisdictions with religious-law-influenced statutes, real legal and social consequences, toward the restrictive final-stage ruling and its certifying authorities.
% ABSENT_VOICES: Communities whose local practice depends on early permissive texts (particularly around intoxicants and certain marriage/testimony provisions) are rarely represented in the seminary councils that adjudicate which reading is taught as authoritative; their practice is characterized by the framework rather than defended within it.
% DISAPPEARANCE_RATIONALE: Proponents of progressive-restriction argue that without this reading, the Quranic corpus would present unresolved internal tension between permissive and restrictive rulings on the same topics, undermining a coherent shariah on issues like intoxicants and inheritance. Rival-school scholars and affected communities argue the world would barely change in substance — the same restrictive rulings are independently derivable via classical abrogation or contextual harmonization — but the institutional authority currently vested in 'divine pedagogy' narrators would dissolve, redistributing interpretive legitimacy to competing schools.
% FOUNDING_PROBLEM: Early Muslim jurists faced Quranic verses on the same topics (intoxicants, testimony, marriage, warfare conduct) that appeared to move from permissive to restrictive across the revelatory timeline, requiring some principle to determine which ruling binds contemporary practice.
% FOUNDING_PROBLEM_CORROBORATION: Evolutionary legal scholars and pedagogical institutions attest the founding problem remains live — new circumstances continually require locating where an issue sits on the permissive-to-restrictive arc. Classical abrogation scholars and contextual harmonization scholars, from outside the progressive-restriction beneficiary set, attest that the founding problem was already fully addressed by their own competing methodologies centuries ago and that the progressive-restriction frame is a later theoretical overlay rather than a response to any unresolved need; comparative religious historians corroborate that all three frameworks emerged in overlapping historical periods as competing solutions to the same textual tension, not sequentially as fix-then-obsolescence.
narrative_ontology:disappearance_verdict(naskh_principle__progressive_restriction, contested).
narrative_ontology:founding_problem_status(naskh_principle__progressive_restriction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__progressive_restriction, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(naskh_principle__progressive_restriction, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__progressive_restriction, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__progressive_restriction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__progressive_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__progressive_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate: the reading does not primarily transfer material wealth, but it transfers interpretive legitimacy and practical legal deference away from communities that rely on now-recharacterized permissive texts, with measurable downstream effects wherever religious-law-influenced statutes attach consequences to that recharacterization. Suppression (0.58) is higher than extraction because maintaining the pedagogical-arc reading as authoritative requires active curricular and institutional gatekeeping against both rival kernel readings and lay reliance on permissive texts. Theater ratio (0.30) reflects that a substantial share of the doctrine's genuine coordination function (resolving real textual tension for legal derivation) persists alongside a growing performative layer of institutional certification. Accessibility collapse (0.48) is moderate-low: rival readings remain intellectually accessible and actively taught elsewhere, so alternatives have not fully collapsed. Resistance (0.55) is substantial: both rival scholarly traditions and affected lay communities actively contest the progressive-restriction narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   Evolutionary legal scholars and pedagogical authority institutions sit near the beneficiary end: they administer the doctrine, gain interpretive latitude and gatekeeping authority from it, and face no personal cost from its adoption. Practitioners citing early permissive verses and communities relying on intoxicant-leniency texts sit near the target end: their reliance on textual grounds is delegitimized by the doctrine's own logic, and their exit options are constrained or trapped by locality and social embeddedness. Literalist minority schools are powerless and trapped precisely because the entire premise of graduated pedagogy is foreign to their hermeneutic — they cannot simply 'exit' into a rival reading without abandoning their tradition's core commitment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling permissive/restrictive verse-pairs for legal derivation) is contested as live vs. dead: corroboration from outside the beneficiary set (rival schools, historians) suggests the problem was independently solved by other methodologies at the same historical moment progressive-restriction emerged, meaning the doctrine's persistence is not straightforwardly explained by ongoing necessity. Classifying this as tangled_rope rather than snare or mountain prevents two errors: treating it as pure extraction (it does perform a genuine, non-trivial coordination function — resolving textual tension into usable law) while also refusing to treat it as natural/inevitable (it is one of three live, mutually exclusive methodological choices, each with its own beneficiaries).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogy_vs_invalidation_ambiguity,
    'Is ''divine pedagogy'' a genuinely distinct hermeneutic category from both formal abrogation and contextual harmonization, or is it a relabeling of one of the other two positions that preserves interpretive flexibility for whoever narrates the pedagogical stages?',
    'Close textual-historical analysis of whether progressive-restriction proponents produce different legal outcomes than classical abrogationists on contested cases (e.g., intoxicants, testimony), or whether the outcomes converge and only the justificatory narrative differs.',
    'If outcomes converge with classical_abrogation, progressive_restriction functions as a rhetorical variant rather than a structurally distinct reading, reducing its independent extraction to primarily narrative/legitimation value rather than substantive legal divergence. If outcomes diverge, it is a genuinely separate constraint with its own legal consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogy_vs_invalidation_ambiguity, conceptual, 'Whether progressive-restriction is doctrinally distinct from classical abrogation in practical legal effect.').

omega_variable(
    kernel_reading_selection_mechanism,
    'What determines which of the three naskh readings a given seminary, fatwa council, or state religious authority adopts as its taught doctrine — textual-scholarly merit, institutional path-dependency, political convenience, or some mixture?',
    'Comparative institutional history tracing adoption of each reading across major fiqh schools and state religious authorities, correlated with the material and political interests of the adopting institutions.',
    'If adoption correlates strongly with institutional interest rather than scholarly merit, this strengthens the tangled_rope classification (genuine coordination function captured by interested parties); if adoption correlates with textual-critical scholarship independent of institutional interest, the extraction component is weaker than authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, empirical, 'Whether reading-selection across institutions tracks scholarly merit or institutional interest.').

omega_variable(
    corroboration_asymmetry,
    'Given that rival-school scholars attest the founding problem is already solved by their own methods, is their corroboration itself interest-laden (defending their own school''s relevance) in the same way the progressive-restriction proponents'' self-attestation would be?',
    'Triangulate with the comparative religious historian seat''s independent account of why three readings emerged roughly contemporaneously rather than sequentially.',
    'If rival-school corroboration is equally interest-laden, the founding_problem_status assessment should weight historian testimony more heavily than either scholarly faction''s account, tempering the ''dead problem'' reading somewhat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corroboration_asymmetry, conceptual, 'Whether outside-corroboration from rival schools is itself sufficiently disinterested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__progressive_restriction, theater_ratio, 0, 0.18).
narrative_ontology:measurement(nask_tr_t20, naskh_principle__progressive_restriction, theater_ratio, 20, 0.21).
narrative_ontology:measurement(nask_tr_t40, naskh_principle__progressive_restriction, theater_ratio, 40, 0.24).
narrative_ontology:measurement(nask_tr_t60, naskh_principle__progressive_restriction, theater_ratio, 60, 0.26).
narrative_ontology:measurement(nask_tr_t80, naskh_principle__progressive_restriction, theater_ratio, 80, 0.28).
narrative_ontology:measurement(nask_tr_t100, naskh_principle__progressive_restriction, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__progressive_restriction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nask_be_t20, naskh_principle__progressive_restriction, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(nask_be_t40, naskh_principle__progressive_restriction, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(nask_be_t60, naskh_principle__progressive_restriction, base_extractiveness, 60, 0.47).
narrative_ontology:measurement(nask_be_t80, naskh_principle__progressive_restriction, base_extractiveness, 80, 0.5).
narrative_ontology:measurement(nask_be_t100, naskh_principle__progressive_restriction, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__progressive_restriction, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(nask_su_t20, naskh_principle__progressive_restriction, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(nask_su_t40, naskh_principle__progressive_restriction, suppression_requirement, 40, 0.49).
narrative_ontology:measurement(nask_su_t60, naskh_principle__progressive_restriction, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(nask_su_t80, naskh_principle__progressive_restriction, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(nask_su_t100, naskh_principle__progressive_restriction, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__progressive_restriction, identity_coordination).
narrative_ontology:boltzmann_floor_override(naskh_principle__progressive_restriction, 0.1).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__contextual_harmonization).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language concept 'naskh' (Quranic abrogation) per the ε-invariance principle: classical_abrogation (formal verse-pair supersession by chronology), contextual_harmonization (all verses independently valid within context, no supersession), and progressive_restriction (sequence itself carries pedagogical weight, a middle position). Each has a distinct beneficiary/victim structure and its own ε; none is a parameterization of the others. The kernel is the underlying commitment (how to handle apparently conflicting revealed rulings); each reading is a separate, live, coexisting position within Islamic legal theory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
