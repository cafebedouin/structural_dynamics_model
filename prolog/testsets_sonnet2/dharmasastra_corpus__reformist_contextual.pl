% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__reformist_contextual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__reformist_contextual, []).

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
 *   constraint_id: dharmasastra_corpus__reformist_contextual
 *   human_readable: Dharmasastra as Historically-Conditioned Ethical Core (Reformist-Contextual Reading)
 *   domain: religious/legal/social
 *
 * SUMMARY:
 *   This story instantiates the reformist-contextual reading of the
 *   Dharmasastra kernel: the claim that the textual corpus's ethical core
 *   (dharma as righteous conduct, moral duty, ritual propriety) is separable
 *   from its time-bound caste and gender prescriptions, which reflect the
 *   historical social conditions of composition rather than eternal revealed
 *   truth. Under this reading, formal caste enforcement recedes (declining
 *   suppression_requirement and base_extractiveness over the interval as
 *   legal caste discrimination is formally abolished and doctrinal
 *   reinterpretation spreads) while theater_ratio rises — public reformist
 *   discourse and institutional reinterpretation increasingly substitute for
 *   structural change in lived caste dynamics, which persist through informal
 *   channels (marriage markets, kinship networks, local custom) that the
 *   doctrinal reframing does not directly touch. This is a Tangled Rope: it
 *   genuinely coordinates a workable ethical framework for a modernizing
 *   religious community (real coordination function) while asymmetrically
 *   preserving status advantages for upper-caste and institutional actors at
 *   the ongoing cost of Dalit communities, lower-caste women, and inter-caste
 *   couples, and it requires active institutional maintenance (reformist
 *   commentary, selective doctrinal emphasis, social enforcement of the
 *   softened norms) to hold the reinterpretation together against both
 *   orthodox and abolitionist pressure.
 *
 * KEY AGENTS:
 *   - reformist_religious_authorities: institutional agenda-setters who administer the reinterpretation
 *   - dalit_communities and lower_caste_women: bear residual, informally-enforced caste harm despite doctrinal softening
 *   - upper_caste_communities_retaining_symbolic_status: retain practical benefit while shedding literalist liability
 *   - orthodox_literalist_clergy and abolitionist_scholars_and_activists: excluded critics from opposite structural directions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, 0.42).
domain_priors:suppression_score(dharmasastra_corpus__reformist_contextual, 0.38).
domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, extractiveness, 0.42).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Dharmasastra as Historically-Conditioned Ethical Core (Reformist-Contextual Reading)").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious/legal/social").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, 'f938af10-9484-444d-8b19-f650e5bed1ae').
narrative_ontology:cs_kernel_codification('f938af10-9484-444d-8b19-f650e5bed1ae', fixed_text).
narrative_ontology:cs_authority_grounding('f938af10-9484-444d-8b19-f650e5bed1ae', lineage).
narrative_ontology:cs_interpretation_layer_present('f938af10-9484-444d-8b19-f650e5bed1ae').
narrative_ontology:cs_reading_relation('f938af10-9484-444d-8b19-f650e5bed1ae', dharmasastra_corpus__orthodox_literalist, coexists_with).
narrative_ontology:cs_reading_relation('f938af10-9484-444d-8b19-f650e5bed1ae', dharmasastra_corpus__abolitionist_rejection, influences).
narrative_ontology:cs_axiom('f938af10-9484-444d-8b19-f650e5bed1ae', foundational, dharma_ethical_core_transcends_historical_prescription).
narrative_ontology:cs_axiom_status(dharma_ethical_core_transcends_historical_prescription, holdable).
narrative_ontology:cs_axiom_grounding('f938af10-9484-444d-8b19-f650e5bed1ae', dharma_ethical_core_transcends_historical_prescription, conventional).
narrative_ontology:cs_axiom('f938af10-9484-444d-8b19-f650e5bed1ae', foundational, caste_prescriptions_are_historically_contingent_not_eternal).
narrative_ontology:cs_axiom_status(caste_prescriptions_are_historically_contingent_not_eternal, holdable).
narrative_ontology:cs_axiom_grounding('f938af10-9484-444d-8b19-f650e5bed1ae', caste_prescriptions_are_historically_contingent_not_eternal, empirically_contingent).
narrative_ontology:cs_reference_frame('f938af10-9484-444d-8b19-f650e5bed1ae', classical_smriti_social_order).
narrative_ontology:cs_drift_state('f938af10-9484-444d-8b19-f650e5bed1ae', post_independence_constitutional_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('f938af10-9484-444d-8b19-f650e5bed1ae', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, reformist_religious_authorities).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, moderate_caste_hindu_institutions).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, upper_caste_communities_retaining_symbolic_status).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, dalit_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, lower_caste_women).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, inter_caste_couples).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, upper_caste_communities_retaining_symbolic_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reinterpret Dharmasastra texts to extract dharma as universal righteous conduct while recasting varna/jati prescriptions as descriptive of a bygone social order rather than binding law. They administer this reading through commentary, sermons, and reform organizations, retaining the textual corpus's authority while selectively discarding its most extractive clauses. Their institutional legitimacy and continued relevance depend on the corpus remaining authoritative in some form.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, reformist_religious_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Temples, educational trusts, and community organizations that adopt the reformist reading to retain congregants and legitimacy amid modern egalitarian pressure. They benefit from a version of tradition that preserves ritual continuity and cultural identity without the reputational cost of defending literal caste hierarchy.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, moderate_caste_hindu_institutions, beneficiary,
    organized, generational, constrained, national).

% Retain informal social capital, marriage-network advantages, and priestly/administrative roles that trace to the same hierarchy the reformist reading claims to have softened. They pay a reputational cost (defending a tradition now associated with oppression) but keep most of the practical benefit, since 'spiritual stages' language rarely disturbs actual social networks or endogamy patterns.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, upper_caste_communities_retaining_symbolic_status, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__reformist_contextual, upper_caste_communities_retaining_symbolic_status, payer).

% Continue to face caste-based exclusion in marriage, temple access, and social standing even where the corpus is reinterpreted symbolically rather than literally enforced. The reformist reading removes the legal teeth of the old prescriptions but does not dismantle the underlying status hierarchy that the same corpus originally encoded and that persists through custom, kinship networks, and local enforcement independent of doctrinal reinterpretation.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, dalit_communities, payer,
    powerless, generational, trapped, national).

% Bear compounded burdens where caste and gender prescriptions intersect within the same corpus; the reformist move to reframe caste as 'spiritual stages' typically leaves the gender-hierarchical prescriptions (stridharma, restrictions on widow remarriage, purity codes) less scrutinized because the reform effort's political attention concentrates on caste rather than gender. Exit from either constraint requires leaving the community entirely.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, lower_caste_women, payer,
    powerless, biographical, trapped, regional).

% Face social and sometimes violent sanction for marriages that cross the hierarchy the reformist reading claims is merely symbolic; the gap between the doctrinal reinterpretation and the lived enforcement of endogamy falls directly on them. Their exit option (leaving the natal community) carries severe social and sometimes physical cost.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, inter_caste_couples, payer,
    moderate, biographical, constrained, local).

% Regard the reformist reading as an illegitimate dilution of revealed prescription and are increasingly marginalized in reform-oriented institutional spaces, though they retain influence in traditionalist enclaves. They are excluded from the reformist reading's institutional conversation even though they would contest its core premise directly.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, orthodox_literalist_clergy, excluded,
    organized, generational, constrained, national).

% Argue the reformist move is a legitimacy-laundering exercise that lets the textual corpus and its custodial authorities survive by shedding only their most indefensible clauses while leaving the underlying framework and its beneficiaries largely intact. They are structurally outside the reformist conversation, which treats their rejection of the corpus's authority as illegitimate rather than engaging it directly.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, abolitionist_scholars_and_activists, excluded,
    moderate, generational, mobile, national).

% Study the historical formation of Dharmasastra texts, the social conditions of their composition, and the contemporary reinterpretive movements as data on how legal-religious corpora adapt to changed normative environments.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a continuous ethical and cultural framework — dharma as righteous conduct, ritual obligation, and social order — that Hindu communities can use to organize religious practice, kinship norms, and moral education without discarding a centuries-old textual tradition wholesale.
% TRANSFER_FUNCTION: Moves social status, marriage-market position, and access to ritual/institutional roles from those positioned lower in the historically-encoded hierarchy to those positioned higher, while relabeling the mechanism as 'spiritual stage' rather than binding caste law; also moves reputational capital to reformist authorities who retain institutional relevance by managing the reinterpretation.
% ABSENT_VOICES: Dalit communities and lower-caste women whose lived exclusion persists largely unchanged by doctrinal reinterpretation are rarely the ones authoring or ratifying the reformist reading; the reinterpretation is substantially produced by upper-caste reformist scholars and institutions speaking on their behalf. Orthodox literalists and abolitionists are both excluded from the reformist institutional conversation despite representing the sharpest structural critiques from opposite directions.
% DISAPPEARANCE_RATIONALE: Reformist authorities and moderate institutions would argue the world rearranges significantly — a coherent ethical bridge between tradition and egalitarian modernity would vanish, destabilizing religious institutions that depend on it for legitimacy. Abolitionist critics and many Dalit activists would argue the world is largely unchanged for those actually bearing caste harm, since the informal hierarchy persists through kinship, endogamy, and local custom independent of which doctrinal reading is officially favored.
% FOUNDING_PROBLEM: The corpus was compiled to codify dharma — right conduct, ritual duty, and social order — for a specific historical society, integrating caste, gender, and ritual prescriptions into a single normative system presented as cosmically sanctioned.
% FOUNDING_PROBLEM_CORROBORATION: Reformist authorities themselves attest the ethical-core problem (how should one live rightly) remains live while the caste-prescription problem is dead (a historical artifact). Independent historians and sociologists of caste corroborate that caste-based exclusion persists in marriage markets, land access, and social status largely independent of which doctrinal reading is currently favored by religious elites — evidence external to the reformist authorities themselves that the underlying social hierarchy the corpus encoded has not dissolved merely because its textual justification has been reinterpreted.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__reformist_contextual, contested).
narrative_ontology:founding_problem_status(dharmasastra_corpus__reformist_contextual, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__reformist_contextual, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dharmasastra_corpus__reformist_contextual, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__reformist_contextual, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__reformist_contextual_tests).
:- end_tests(dharmasastra_corpus__reformist_contextual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at medium (0.42 at interval end, down from 0.62) reflecting the story's own framing: hierarchy persists symbolically rather than through strict legal enforcement, so the referent — the standing arrangement under contest, as this reading itself construes it — genuinely does show reduced formal extraction relative to the literalist baseline. Suppression is moderate and declining (0.38 at end) because active coercive enforcement of caste boundaries by religious authority has weakened, though it has not disappeared, and much of what remains is social rather than doctrinal. Theater ratio is authored rising to 0.5 because an increasing share of the reformist apparatus's activity is public reinterpretation and moral rebranding rather than material change in caste-linked outcomes — this is the honest cost of the reading's own strategy of preserving textual authority while discarding oppressive elements: the preservation effort itself becomes increasingly performative as the gap between doctrine and lived hierarchy widens.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist authorities and moderate institutions sit near the beneficiary end: they retain relevance, legitimacy, and institutional continuity by managing the reinterpretation, with arbitrage-grade exit (they can recast doctrine as needed). Upper-caste communities are dual-positioned — beneficiaries of retained informal status, but payers of reputational cost for defending an association with a tradition now criticized as oppressive; their mobile exit options let them absorb this cost more easily than lower-caste payers. Dalit communities, lower-caste women, and inter-caste couples sit near the target end: trapped or constrained exit, generational or biographical time horizon, and they bear the persistence of informal hierarchy that formal doctrinal softening does not reach.
 *
 * MANDATROPHY ANALYSIS:
 *   The reformist-contextual reading is precisely a mandatrophy-management move: it attempts to resolve the mandatrophy of a corpus whose founding problem (codifying a historical social order as cosmic law) is largely dead while its institutional apparatus persists, by declaring the ethical-core function still live and severing it from the dead caste-prescription function. Classifying this as tangled_rope rather than either mountain (which would falsely naturalize the residual hierarchy) or pure snare (which would deny the genuine ethical-coordination function reformist communities do use) avoids both mislabeling errors: the coordination function is real, but so is the asymmetric extraction that survives the reinterpretation via informal channels the doctrine does not govern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethical_core_separability,
    'Is the ''ethical core'' of dharma as righteous conduct genuinely separable from the caste and gender prescriptions within the same textual corpus, or are they so structurally interwoven in the source texts that extracting one from the other is itself an interpretive imposition rather than a discovery?',
    'Close philological and historical analysis of whether major Dharmasastra texts (Manusmriti, Yajnavalkya Smriti, etc.) present dharma as a unified system where caste duty (varnashrama dharma) is constitutive of righteous conduct rather than incidental to it, versus textual layers or strata that can be independently dated and attributed to different historical redactions.',
    'If the prescriptions are constitutively fused with the ethical core in the source texts, the reformist separability claim is itself a modern interpretive overlay rather than a recovery of original intent, which would strengthen the abolitionist reading''s charge of legitimacy-laundering. If the texts show genuine stratification supporting separability, the reformist reading''s core premise is textually grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_core_separability, conceptual, 'Whether the reformist separation of ethical core from caste prescription is textually grounded or an interpretive imposition.').

omega_variable(
    kernel_authority_persistence_mechanism,
    'Does the reformist reading''s success in softening caste enforcement at the doctrinal level meaningfully reduce lived caste harm, or does it primarily relocate the corpus''s authority-preserving function from doctrine to informal custom, leaving the same beneficiary structure intact under a different justificatory vocabulary?',
    'Longitudinal sociological data comparing caste-based outcomes (marriage endogamy rates, land access, occupational mobility, incidence of caste-based violence) in communities where reformist doctrine has been institutionally adopted versus communities retaining more literalist framings, controlling for other modernization variables.',
    'If outcomes converge regardless of doctrinal reading, this supports the theater_ratio trajectory authored here (rising performative reinterpretation, static underlying harm) and would support reclassifying the reading''s function as substantially symbolic. If outcomes diverge meaningfully by doctrinal adoption, the reformist reading''s coordination function is doing real extraction-reducing work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_authority_persistence_mechanism, empirical, 'Whether doctrinal reform produces measurable reduction in lived caste-based harm or primarily relocates justification.').

omega_variable(
    framing_choice_kernel_vs_authority_narrative,
    'Should the kernel for this family be framed as ''the Dharmasastra corpus itself'' (the obvious framing used here) or as ''the claim that Dharmasastra authority legitimately survives modern scrutiny'' (a framing one level up, treating textual authority itself as the contested commitment)? Under the first framing, all three readings share a text-interpretation kernel; under the second, the reformist and orthodox readings share a pro-authority-survival commitment that the abolitionist reading rejects at a different structural level.',
    'Compare whether cs_pattern classification differs meaningfully between the two framings — e.g., whether the reformist/orthodox pairing under the authority-survival framing produces a coexists_with relation with different axiom grounding than the direct textual-kernel framing used here.',
    'The textual-kernel framing (used in this story) treats all three readings as commensurable interpretations of one text; the authority-survival framing would treat the abolitionist reading as operating at a categorically different level (rejecting the authority question itself rather than offering a competing interpretation), which could change whether abolitionist_rejection is best modeled as ''forecloses'' rather than ''coexists_with'' relative to this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_choice_kernel_vs_authority_narrative, conceptual, 'Alternative framing of the kernel as textual corpus versus authority-survival claim, and its effect on cross-reading relations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__reformist_contextual, theater_ratio, 0, 0.3).
narrative_ontology:measurement(dhar_tr_t10, dharmasastra_corpus__reformist_contextual, theater_ratio, 10, 0.35).
narrative_ontology:measurement(dhar_tr_t20, dharmasastra_corpus__reformist_contextual, theater_ratio, 20, 0.4).
narrative_ontology:measurement(dhar_tr_t30, dharmasastra_corpus__reformist_contextual, theater_ratio, 30, 0.44).
narrative_ontology:measurement(dhar_tr_t40, dharmasastra_corpus__reformist_contextual, theater_ratio, 40, 0.47).
narrative_ontology:measurement(dhar_tr_t50, dharmasastra_corpus__reformist_contextual, theater_ratio, 50, 0.49).
narrative_ontology:measurement(dhar_tr_t60, dharmasastra_corpus__reformist_contextual, theater_ratio, 60, 0.5).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__reformist_contextual, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(dhar_be_t10, dharmasastra_corpus__reformist_contextual, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(dhar_be_t20, dharmasastra_corpus__reformist_contextual, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(dhar_be_t30, dharmasastra_corpus__reformist_contextual, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(dhar_be_t40, dharmasastra_corpus__reformist_contextual, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(dhar_be_t50, dharmasastra_corpus__reformist_contextual, base_extractiveness, 50, 0.43).
narrative_ontology:measurement(dhar_be_t60, dharmasastra_corpus__reformist_contextual, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__reformist_contextual, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dhar_su_t10, dharmasastra_corpus__reformist_contextual, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(dhar_su_t20, dharmasastra_corpus__reformist_contextual, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(dhar_su_t30, dharmasastra_corpus__reformist_contextual, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(dhar_su_t40, dharmasastra_corpus__reformist_contextual, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(dhar_su_t50, dharmasastra_corpus__reformist_contextual, suppression_requirement, 50, 0.38).
narrative_ontology:measurement(dhar_su_t60, dharmasastra_corpus__reformist_contextual, suppression_requirement, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__reformist_contextual, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the dharmasastra_corpus kernel. orthodox_literalist authors near-zero extraction (the hierarchy is not extractive from within that reading's own premises — it is cosmically sanctioned order) and a much higher suppression/enforcement profile aimed at maintaining literal observance. abolitionist_rejection authors high extraction and treats the entire corpus, including any claimed 'ethical core,' as illegitimate, with victim sets not softened by symbolic reinterpretation. This reformist_contextual reading sits structurally between them: medium, declining extraction, a reduced but non-empty victim set, and a genuine (if contested) coordination function. Each story's ε is authored independently from that reading's own construal of the standing arrangement, per the kernel-reading ε referent rule — they are not the same measurement taken from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
