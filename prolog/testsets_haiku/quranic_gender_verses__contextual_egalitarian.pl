% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__contextual_egalitarian
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__contextual_egalitarian, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quranic_gender_verses__contextual_egalitarian
 *   human_readable: Qur'anic Gender Verses as Contextual Egalitarian Reading
 *   domain: religious/legal/gender
 *
 * SUMMARY:
 *   The contextual-egalitarian reading of Qur'anic gender verses (4:11
 *   inheritance, 2:282 testimony, 4:34 guardianship) frames these verses as
 *   historically situated responses to 7th-century Arabian conditions,
 *   requiring reinterpretation under the overarching Islamic principle of
 *   maqasid (higher objectives: justice, human dignity, welfare). This
 *   reading is ONE of three contested readings of the same kernel—the
 *   Qur'an's actual gender prescriptions. Under this reading, women
 *   transition from a structural victim set (differential legal standing) to
 *   a beneficiary set (equal rights grounded in theological
 *   reinterpretation). The reading gains traction through reformist
 *   scholarship and international rights organizations, which erodes the
 *   interpretive authority of patriarchal legal traditions. The constraint's
 *   operation is moderately extractive: it extracts discretionary power from
 *   traditional judges and scholars, who lose the ability to cite verses as
 *   binding proof-texts for gender differentiation. It requires active
 *   enforcement because literal-reading interpreters resist the hermeneutic
 *   shift and must be continually challenged; suppression is high because the
 *   reading works partly by delegitimizing competing interpretations rather
 *   than by purely theological argument. Theater is moderate because
 *   performative theological debate (counter-interpretations, competing
 *   fatwas) now absorbs significant institutional energy on both sides.
 *
 * KEY AGENTS:
 *   - Reformist scholars: institutional power, set the hermeneutic standard via maqasid framework; gain interpretive authority and theological legitimacy
 *   - Rights-based NGOs: organized power, provide organizational bandwidth for the reading's dissemination and policy implementation; beneficiary-adjacent through legitimacy gain
 *   - Women as legal agents: gain structural claims to equal inheritance, testimony, freedom from guardianship; move from victim to beneficiary set
 *   - Patriarchal legal interpreters: institutional power, lose discretionary authority; forced into defensive hermeneutical posture
 *   - Traditional judicial authority: institutional power, constrained by reformist reinterpretations of their foundational texts; cannot exit without dismantling their authority structure
 *   - Conservative theological institutions: identity-locked to literal readings; forced into performative defense (competing fatwas) to maintain institutional identity
 *   - Literal-hierarchical interpreters: excluded from the legitimacy framework; their core premise (timeless male guardianship) is treated as non-hermeneutical rather than as a coherent theological option
 *   - Abrogation-principle advocates: excluded from the framework; their alternative reading (abrogation rather than contextualization) is not acknowledged as legitimate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__contextual_egalitarian, 0.62).
domain_priors:suppression_score(quranic_gender_verses__contextual_egalitarian, 0.71).
domain_priors:theater_ratio(quranic_gender_verses__contextual_egalitarian, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, extractiveness, 0.62).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__contextual_egalitarian, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__contextual_egalitarian, "Qur'anic Gender Verses as Contextual Egalitarian Reading").
narrative_ontology:topic_domain(quranic_gender_verses__contextual_egalitarian, "religious/legal/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__contextual_egalitarian).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__contextual_egalitarian, '35533657-b1e5-4610-80c5-9e64444c1a8f').
narrative_ontology:cs_kernel_codification('35533657-b1e5-4610-80c5-9e64444c1a8f', fixed_text).
narrative_ontology:cs_authority_grounding('35533657-b1e5-4610-80c5-9e64444c1a8f', lineage).
narrative_ontology:cs_interpretation_layer_present('35533657-b1e5-4610-80c5-9e64444c1a8f').
narrative_ontology:cs_reading_relation('35533657-b1e5-4610-80c5-9e64444c1a8f', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('35533657-b1e5-4610-80c5-9e64444c1a8f', quranic_gender_verses__progressive_abrogation, coexists_with).
narrative_ontology:cs_axiom('35533657-b1e5-4610-80c5-9e64444c1a8f', foundational, verses_reflect_historical_context).
narrative_ontology:cs_axiom_status(verses_reflect_historical_context, holdable).
narrative_ontology:cs_axiom_grounding('35533657-b1e5-4610-80c5-9e64444c1a8f', verses_reflect_historical_context, empirically_contingent).
narrative_ontology:cs_axiom('35533657-b1e5-4610-80c5-9e64444c1a8f', foundational, maqasid_framework_recovers_quranic_intent).
narrative_ontology:cs_axiom_status(maqasid_framework_recovers_quranic_intent, holdable).
narrative_ontology:cs_axiom_grounding('35533657-b1e5-4610-80c5-9e64444c1a8f', maqasid_framework_recovers_quranic_intent, deontological).
narrative_ontology:cs_axiom('35533657-b1e5-4610-80c5-9e64444c1a8f', secondary, gender_equality_compatible_with_islamic_law).
narrative_ontology:cs_axiom_status(gender_equality_compatible_with_islamic_law, holdable).
narrative_ontology:cs_axiom_grounding('35533657-b1e5-4610-80c5-9e64444c1a8f', gender_equality_compatible_with_islamic_law, instrumental).
narrative_ontology:cs_reference_frame('35533657-b1e5-4610-80c5-9e64444c1a8f', qur_an_as_timeless_divine_ordinance).
narrative_ontology:cs_drift_state('35533657-b1e5-4610-80c5-9e64444c1a8f', contemporary_reformist_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('35533657-b1e5-4610-80c5-9e64444c1a8f', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, reformist_scholars).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, rights_based_ngos).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, women_as_equal_moral_agents).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, patriarchal_legal_interpreters).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, traditional_judicial_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, conservative_theological_institutions).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, maqasid_al_shariah_framework).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, progressive_revelation_principle).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, contextual_hermeneutics_in_sacred_texts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academics and theologians who assert that Qur'anic verses on gender (4:11 inheritance, 2:282 testimony, 4:34 guardianship) were historically situated responses to 7th-century Arabian context, not timeless prescriptions. They develop interpretive methodologies using maqasid (higher objectives of Islamic law) to reframe these verses as progressive for their era while supporting modern gender equality. They gain authority by framing themselves as recovering the Qur'an's deeper equity principles; this redeploys the text's legitimacy toward their readings and away from literal-hierarchical interpreters. They author the hermeneutic framework that constrains how traditional courts can credibly cite these verses.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, reformist_scholars, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, reformist_scholars, beneficiary).

% International and local organizations advocating women's inheritance equality, testimony parity, and freedom from male guardianship. The contextual-egalitarian reading provides theological legitimacy for their policy demands; they cite reformist scholarship to argue that gender-equal Islamic law is not un-Islamic but rather a recovery of the Qur'an's genuine intent. They gain structural power by being able to claim Qur'anic authority rather than merely secular authority.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, rights_based_ngos, beneficiary,
    organized, biographical, mobile, global).

% Women whose legal standing and rights are substantively expanded under the contextual-egalitarian reading: they become eligible for equal inheritance (not half-shares), their testimony weights equally in court (not half-value), and they are no longer structural subjects of male guardianship. The constraint shifts them from a victim set under literal-hierarchical reading to a beneficiary set here. Their exit from the reading is constrained by the surrounding legal order; their mobility depends on whether states and communities adopt the reformist interpretation.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, women_as_equal_moral_agents, beneficiary,
    moderate, biographical, constrained, global).

% Traditional Islamic scholars and judges whose authority derives from literal readings of gender-specific verses. Under the contextual-egalitarian reading, their interpretive monopoly erodes: the same verses they cite as proof-texts become evidence of historical context rather than timeless law. They lose discretionary power to issue fatwas grounding guardianship, differential testimony, and inheritance rules in divine ordinance. Their response is to defend literalism as the only legitimate hermeneutics; this constraint forces them into an active defense that requires suppressing or discrediting reformist scholarship.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, patriarchal_legal_interpreters, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, patriarchal_legal_interpreters, agenda_setter).

% Courts and quasi-judicial bodies (religious tribunals, family law courts in Muslim-majority states) that historically administered gender-differentiated rights using verses 4:11, 2:282, 4:34 as binding precedent. The contextual-egalitarian reading forces them to either reinterpret their foundational texts or cede legitimacy to reformist judges and scholars. Where the reading gains traction, their rulings face challenges and reversals. They cannot exit without dismantling the legal authority they depend on.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, traditional_judicial_authority, payer,
    institutional, generational, constrained, regional).

% Seminaries, teaching bodies, and state-backed religious authorities that have institutionalized literal readings of gender verses into curricula and fatwa bodies. They are identity-locked to their doctrinal position: admitting contextual-egalitarian readings would require foundational revision of their theological training, authority claims, and institutional mission. They experience the constraint as a delegitimization of their core function, forcing performative defense (producing counter-interpretations, issuing competing fatwas) to maintain institutional identity.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, conservative_theological_institutions, payer,
    institutional, generational, identity_locked, regional).

% States with Muslim populations that maintain dual legal systems (civil code + Islamic family law). They observe the constraint as shifting legitimacy conditions: they can now cite Islamic authority (reformist scholars) to justify gender-equal civil codes, reducing the need for secular-versus-religious framing. They gain legitimacy flexibility by absorbing reformist readings into state policy, though this creates tension with conservative constituencies.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, secular_governments, observer,
    institutional, generational, arbitrage, national).

% Scholars who hold that verses 4:11, 2:282, 4:34 are direct, binding, timeless divine law establishing male guardianship and gender differentiation. They are excluded from the contextual-egalitarian reading's legitimacy framework—the reading does not acknowledge their core premise (that these verses are context-independent prescriptions) as a coherent option within Islamic theology. Their objections are framed as refusing to engage with hermeneutical methodology rather than as legitimate theological alternatives.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, literal_hierarchical_interpreters, excluded,
    institutional, generational, constrained, global).

% Scholars who argue that gender-specific verses were valid for their era but have been superseded (abrogated, naskh) by later egalitarian verses (49:13, 3:195, others). They are excluded from the contextual-egalitarian framework because that framework does not accept abrogation—it reframes the verses as contextually appropriate rather than abrogated. The two readings coexist in debate but do not acknowledge each other's premises as legitimate within the same hermeneutic system.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, abrogation_principle_advocates, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__contextual_egalitarian, reformist_scholars).
narrative_ontology:fixing_cost_class(quranic_gender_verses__contextual_egalitarian, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a theological framework (maqasid / higher objectives) for Muslim communities to reinterpret Qur'anic gender verses in light of 7th-century context while maintaining scriptural fidelity. Solves the genuine coordination problem: how to be authentically Muslim and authentically egalitarian simultaneously, rather than choosing between religious tradition and gender equality. Coordinates reformist scholars, rights organizations, and egalitarian-minded Muslims around a shared hermeneutic methodology.
% TRANSFER_FUNCTION: Moves interpretive authority from literal-reading scholars and traditional judges (who command the reading of gender verses for family law) to reformist scholars and rights-based organizations (who set the contextual-hermeneutic standard). Moves women from a legal victim set (inferior inheritance, testimony, guardianship status) to a beneficiary set (equal rights grounded in theological reinterpretation). Transfers legitimacy from patriarchal institutional authority to egalitarian institutional authority, using the Qur'an itself as the legitimizing vehicle.
% ABSENT_VOICES: Literal-hierarchical interpreters are excluded from the theological legitimacy framework—their core premise (timeless male guardianship ordinance) is treated as non-hermeneutical rather than as a coherent theological option. Women whose religious commitments bind them to literal readings are structurally absent from reformist institutions. Conservative women (mothers-in-law, female traditional scholars) whose authority derives from patriarchal family structures are not represented in the reformist conversation. Lay Muslim women in conservative communities, whose lived experience of the literal reading may differ sharply from both reformist and literal scholarly accounts, are absent from both sides of the theological debate.
% DISAPPEARANCE_RATIONALE: If the contextual-egalitarian reading and maqasid framework disappeared, Muslim-majority jurisdictions would revert to literal readings of gender verses for family law; women's inheritance would revert to half-share, testimony to half-weight, guardianship to male authority; family law across Muslim-majority countries would align with patriarchal interpretation; international human rights law would lose a major legitimacy claim (Islamic authority for gender equality); and women's movements within Muslim communities would lose theological grounding for their demands. Entire legal systems in multiple countries have restructured around the availability of the contextual reading; institutional actors (judges, lawmakers, scholars) have positioned themselves in relation to it; removing it would require comprehensive legal and theological reorganization.
% FOUNDING_PROBLEM: How can Muslim communities and legal systems maintain both scriptural fidelity to the Qur'an and gender equality in law? How can 7th-century verses on inheritance, testimony, and guardianship be coherently interpreted in light of modernity without either abandoning the Qur'an or accepting patriarchal law?
% FOUNDING_PROBLEM_CORROBORATION: Reformist scholars and rights-based organizations across Muslim-majority countries attest the founding problem is live and urgent—communities continually confront the tension between inherited patriarchal family law and modern egalitarian values. Secular human rights organizations and comparative law scholars affirm the problem's reality and the contextual-egalitarian reading's coherence as a theological solution. Conservative scholars attest the problem is a false framing—the verses are timeless and egalitarian values should be rejected in favor of Islamic law. International human rights monitoring bodies (UN, regional human rights commissions) document the real-world stakes: women's inheritance, testimony, and guardianship status affect millions of legal decisions across Muslim-majority jurisdictions. This external corroboration from human rights bodies, independent scholars, and reformist institutions outside the benefiting countries affirms both the problem's reality and the contextual reading's relevance.
narrative_ontology:disappearance_verdict(quranic_gender_verses__contextual_egalitarian, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__contextual_egalitarian, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__contextual_egalitarian, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__contextual_egalitarian, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__contextual_egalitarian_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__contextual_egalitarian, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__contextual_egalitarian_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness rises over the 100-year interval (1924–2024) as the contextual-egalitarian reading accumulates institutional support and hermeneutic authority. At t0 (1924, post-Ottoman period), the reading barely existed; literal hierarchical readings dominated judicial and scholarly authority. By t1 (1960s, post-independence), early reformist scholars (Abduh, Rida) had established the principle of contextual interpretation, and extractiveness begins to rise as women and reform movements cite these readings. By t2 (1990s, post-Cold War), the reading is institutionalized in international human rights law and comparative legal scholarship; extractiveness rises sharply as the reading gains external legitimacy. By t3 (2010s), the maqasid framework is taught in universities and cited by judges; extractiveness stabilizes as the reading becomes sufficiently entrenched that suppression, though still required, faces diminishing returns. Suppression requirement is high because literal interpreters mount sustained counter-arguments (publishing, teaching, fatwa issuance) and because conservative constituencies mobilize resistance. Theater ratio rises because an increasing share of institutional energy is devoted to performative theological argument (competing interpretations, fatwa exchanges) rather than substantive enforcement of women's rights—the theater indexes the ongoing legitimacy contest. Accessibility of alternatives: women and reformists can now cite the Qur'an itself (via contextual reading) rather than relying on secular authority, which collapses the accessibility of literal-only interpretations for those seeking gender equality within a theological framework.
 *
 * PERSPECTIVAL GAP:
 *   From the reformist scholarly seat, the constraint is a solution to a genuine theological problem—how to maintain scriptural fidelity while adapting to modern conditions. From the patriarchal judicial seat, it is an imposed hermeneutical framework that strips them of interpretive authority without their consent. From the women's seat, it is a gain in legal standing but only conditional on community adoption—it is not a direct transfer like the authority extraction, but a conditional benefit that depends on whether judges and lawmakers embrace the reading. From the secular government seat, it is a legitimacy resource—the state can now cite Islamic authority (reformist scholars) for gender-equal law, reducing secular-versus-religious framing. The engine should register this perspectival variance in per-seat classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: beneficiaries include reformist scholars (institutional power, arbitrage exit—they choose the reading for its theological elegance and authority gains; they could exit to literalism or other frameworks; d ≈ 0.20), rights-based NGOs (organized power, mobile exit—they benefit from Islamic legitimacy but could use secular arguments; d ≈ 0.25), and women (moderate power, constrained exit—they benefit substantively but cannot leave Islamic tradition; d ≈ 0.50). Victims include patriarchal legal interpreters (institutional power, constrained exit—they lose interpretive monopoly; d ≈ 0.85) and traditional judicial authority (institutional power, constrained exit—same; d ≈ 0.85). Conservative theological institutions (institutional power, identity-locked—cannot reinterpret without foundational revision; d ≈ 0.80). The average beneficiary directionality is ~0.32; the average victim directionality is ~0.83. This asymmetry drives the tangled-rope classification: there is real coordination function (solving the scriptural-fidelity problem) but also clear extraction (transfer of authority from patriarchal to reformist seats). The constraint requires active enforcement because literal interpreters continuously challenge the contextual reading and must be suppressed via countervailing scholarship, institutional positioning, and state support for reformist jurists.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (scriptural fidelity + modern adaptation) is live and urgent—it has not been solved, only reframed through competing readings. Under the contextual-egalitarian reading, the problem is being addressed, but the addressing mechanism (the reading itself) is contested by literal interpreters who deny that the problem is real in the first place (they claim verses are timeless and need no adaptation). The mandate for the reading is not degraded; it is active but under siege. However, there is a secondary mandatrophy risk: if the reading becomes primarily a tool for authority transfer (extracting power from patriarchs to reformists) rather than genuinely solving the theological problem, then theater_ratio would rise sharply and the reading would degrade into piton (performative defense of egalitarian interpretations without substantive legal change). The measurement series shows theater_ratio rising (from 0.12 to 0.48), which suggests this risk is present—increasingly, the reading's operation is consumed by performative theological debate rather than substantive legal reform. A piton reading would be one where women's legal standing fails to materialize despite the reformist reading's prevalence; the reading persists performatively (scholars argue, fatwas circulate) but judges and lawmakers do not implement it. Currently (2024), the reading is tangled rope because it does coordinate a genuine theological function while extracting authority; if implementation continues to lag behind performative adoption, it risks sliding toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maqasid_framework_stability,
    'Is the maqasid (higher objectives) framework a stable, canonical hermeneutic principle, or is it itself a modern invention that reformist scholars are retroactively attributing to Islamic tradition?',
    'Textual and historical analysis of maqasid''s presence in classical Islamic jurisprudence (al-Shatibi, Ibn Qayyim al-Jawziyya) versus its development in modern reformist scholarship. Comparative examination of how maqasid is invoked across different schools of Islamic law.',
    'If maqasid is a classical principle, the contextual-egalitarian reading is grounded in traditional methodology and has high legitimacy. If it is a modern invention, the reading is itself a reinterpretation dressed in classical garments, which would undermine its claim to recover ''original'' intent and would support literal interpreters'' argument that reformists are simply imposing modern values on the text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maqasid_framework_stability, empirical, 'Whether maqasid is a classical Islamic hermeneutic principle or a modern reformist invention.').

omega_variable(
    historical_context_reconstruction,
    'What was the actual social and legal status of women in 7th-century Arabia, and how do the Qur''anic verses relate to it—as progressive improvements, as accommodations to existing patriarchy, or as expressions of a genuinely different social framework?',
    'Interdisciplinary scholarship combining Arabian historical sources, gender studies, and comparative ancient legal codes; cross-examination of reformist and literal scholars'' historical claims by secular historians.',
    'If verses improved women''s status relative to 7th-century norms (contextual-egalitarian framing), the reading is descriptively accurate. If verses merely accommodated existing patriarchy (literal reading), the reading overstates their progressiveness. If verses assume a social framework fundamentally different from both ancient and modern conditions, the reading''s claim to relevance for modern reform is weakened. This determines whether the reading''s core narrative (progressive revelation) is historically defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_context_reconstruction, empirical, 'Historical accuracy of the claim that gender verses were progressive for their era.').

omega_variable(
    exclusion_of_literal_interpreters,
    'Is the contextual-egalitarian reading''s exclusion of literal-hierarchical interpreters from the theological legitimacy framework justified as a methodological necessity, or is it a strategic silencing of a coherent theological alternative?',
    'Meta-textual analysis of how the reading presents literal interpretation (as non-hermeneutical, as textually indefensible, as obsolete) versus how literal interpreters present their own methodology (as fidelity to divine word, as avoiding human presumption). Examination of whether literal interpretation has internal coherence as a theological system.',
    'If literalism is incoherent or methodologically illegitimate, the exclusion is justified and the reading gains authority. If literalism is a coherent theological alternative with its own legitimacy claims, the exclusion appears as suppression rather than refutation, which undermine the contextual-egalitarian reading''s claim to superior reasoning and reveals the constraint''s extractive character (transfer of authority by delegitimizing competing frameworks rather than by superior argument).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_of_literal_interpreters, conceptual, 'Whether literal interpretation is excluded because it is incoherent or because it is strategically suppressed.').

omega_variable(
    implementation_lag,
    'If the contextual-egalitarian reading is theologically sound and gains institutional support, why do women''s legal rights in Muslim-majority jurisdictions remain substantially unequal to men''s in inheritance, testimony, and guardianship?',
    'Comparative legal analysis of family law statutes in Muslim-majority countries; tracking of which countries have adopted gender-equal inheritance and testimony rules and which have not; interviews with judges and lawmakers about barriers to implementing reformist readings.',
    'If implementation lags despite the reading''s prevalence, the reading may be functioning as theater—high institutional debate and scholarly energy but minimal substantive legal change. This would suggest the reading is serving a legitimacy function for reformist elites rather than a genuine transformation function for women, and would push the constraint toward piton classification (performative defense of egalitarian interpretations without substantive effect). Alternatively, implementation lag might reflect state-level resistance independent of the theological framework, which would preserve the reading''s integrity but reveal the constraint''s limited reach.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_lag, empirical, 'Whether the contextual-egalitarian reading translates into substantive legal equality for women or remains performative.').

omega_variable(
    women_voice_absent,
    'Are women, particularly conservative women and women in traditional communities, genuinely absent from the theological conversation, or are they present but unheard because the debate is controlled by institutional actors?',
    'Ethnographic and qualitative research on women''s own interpretations of gender verses; documentation of women''s arguments in mosque discussions, family contexts, and community debates; analysis of why or how women''s theological contributions are or are not incorporated into reformist scholarship.',
    'If women are genuinely absent, the reading lacks grounding in lived experience and may be solving an elite intellectual problem rather than a real social problem. If women are present but unheard, the reading''s claim to liberation is complicated by its own exclusion of women''s voices—it delivers gender equality to women via elite theological reinterpretation rather than via women''s own hermeneutical authority. This would shift the reading toward snare classification (delivery of outcomes without genuine voice in the process that produces them).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_voice_absent, empirical, 'Whether women are genuinely absent from the theological conversation or present but structurally silenced.').

omega_variable(
    incommensurability_of_readings,
    'Can the contextual-egalitarian reading and the literal-hierarchical reading coexist within a single Islamic theological framework, or do they represent fundamentally incommensurable commitments that cannot both be true in the same community?',
    'Examination of whether a community or an individual can hold both readings simultaneously (e.g., accepting contextual reading for modern law but literal reading for personal piety); comparison with how other theological disagreements (on predestination, anthropomorphism) are managed within Islam.',
    'If the readings are incommensurable, the constraint''s operation requires winner-take-all dynamics—the contextual reading can only gain ground by delegitimizing literalism. If they can coexist (reformists interpret one way, conservatives interpret another, both within Islam), the constraint''s operation is more like competition than extraction, and the tangled-rope classification would become less accurate. The verdict would determine whether the constraint''s suppression mechanism (delegitimizing literal readings) is structurally necessary or strategically chosen.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incommensurability_of_readings, conceptual, 'Whether the contextual-egalitarian and literal-hierarchical readings are logically compatible or fundamentally incommensurable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__contextual_egalitarian, 1924, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t1924, quranic_gender_verses__contextual_egalitarian, theater_ratio, 1924, 0.12).
narrative_ontology:measurement_basis(qura_tr_t1924, projected).
narrative_ontology:measurement(qura_tr_t1960, quranic_gender_verses__contextual_egalitarian, theater_ratio, 1960, 0.22).
narrative_ontology:measurement_basis(qura_tr_t1960, observed).
narrative_ontology:measurement(qura_tr_t1990, quranic_gender_verses__contextual_egalitarian, theater_ratio, 1990, 0.35).
narrative_ontology:measurement_basis(qura_tr_t1990, observed).
narrative_ontology:measurement(qura_tr_t2010, quranic_gender_verses__contextual_egalitarian, theater_ratio, 2010, 0.42).
narrative_ontology:measurement_basis(qura_tr_t2010, observed).
narrative_ontology:measurement(qura_tr_t2024, quranic_gender_verses__contextual_egalitarian, theater_ratio, 2024, 0.48).
narrative_ontology:measurement_basis(qura_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t1924, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 1924, 0.15).
narrative_ontology:measurement_basis(qura_be_t1924, projected).
narrative_ontology:measurement(qura_be_t1960, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement_basis(qura_be_t1960, observed).
narrative_ontology:measurement(qura_be_t1990, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement_basis(qura_be_t1990, observed).
narrative_ontology:measurement(qura_be_t2010, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement_basis(qura_be_t2010, observed).
narrative_ontology:measurement(qura_be_t2024, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(qura_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t1924, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 1924, 0.35).
narrative_ontology:measurement_basis(qura_su_t1924, projected).
narrative_ontology:measurement(qura_su_t1960, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 1960, 0.52).
narrative_ontology:measurement_basis(qura_su_t1960, observed).
narrative_ontology:measurement(qura_su_t1990, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 1990, 0.64).
narrative_ontology:measurement_basis(qura_su_t1990, observed).
narrative_ontology:measurement(qura_su_t2010, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement_basis(qura_su_t2010, observed).
narrative_ontology:measurement(qura_su_t2024, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(qura_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__contextual_egalitarian, identity_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__contextual_egalitarian, 0.12).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__progressive_abrogation).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, islamic_family_law_reform).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, gender_equality_in_inheritance_law).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Qur'anic gender verses kernel. The contextual-egalitarian reading coexists with literal-hierarchical and progressive-abrogation readings, each claiming the same verses as authoritative but interpreting them fundamentally differently. The three readings should be authored as separate constraints with cross-linked affects_constraints declarations; their ε values differ because each reading instantiates a different constraint (different beneficiaries, victims, extraction mechanisms). The kernel_id is shared; the reading_id distinguishes them. The contextual-egalitarian reading focuses on hermeneutical authority transfer and maqasid framework legitimacy. The literal-hierarchical reading focuses on verse-as-timeless-ordinance and male-guardianship authority. The progressive-abrogation reading focuses on later-verses-supersede-earlier and egalitarian-trajectory narratives. Each is ε-invariant and incomparable via observable-switching because they are fundamentally different constraints, not different measurements of the same constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__contextual_egalitarian, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
