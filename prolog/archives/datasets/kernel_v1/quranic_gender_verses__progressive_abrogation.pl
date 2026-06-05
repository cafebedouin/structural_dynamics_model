% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__progressive_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__progressive_abrogation, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: quranic_gender_verses__progressive_abrogation
 *   human_readable: Qur'anic Gender Verses: Progressive Abrogation Reading
 *   domain: islamic_jurisprudence/legal_hermeneutics/gender_studies
 *
 * SUMMARY:
 *   The progressive abrogation reading of Qur'anic gender verses argues that
 *   later egalitarian principles (particularly Qur'an 49:13: 'O mankind,
 *   indeed We have created you from male and female and made you peoples and
 *   tribes that you may know one another. Indeed, the most noble of you in
 *   the sight of Allah is the most righteous of you') supersede and abrogate
 *   earlier gender-specific legal rules (guardianship, testimony weight,
 *   inheritance rules, spousal obedience) through the classical Islamic
 *   jurisprudential principle of naskh (abrogation). This reading is one of
 *   three structurally distinct readings of the contested kernel
 *   'quranic_gender_verses': (1) the literal_hierarchical reading, which
 *   treats gender-specific rules as immutable law; (2) the
 *   contextual_egalitarian reading, which contextualizes gender rules as
 *   historical responses to specific 7th-century Arab conditions; and (3)
 *   this reading—progressive_abrogation—which uses the naskh principle to
 *   treat later egalitarian verses as formally abrogating earlier
 *   hierarchical ones. Each reading instantiates a different constraint
 *   structure with different beneficiaries, victims, and extractiveness
 *   profiles. The progressive abrogation reading exhibits high extractiveness
 *   (0.78) because it delegates traditional authority structures to the
 *   status of 'abrogated law' and imposes complete normative reversal on
 *   communities whose identity is constituted through literal reading. The
 *   suppression is high because adopting this reading within traditional
 *   institutions creates career and community costs; suppression of the
 *   reading itself by traditionalist structures is substantial. The theater
 *   ratio has declined over the interval as the reading has matured
 *   hermeneutically — earlier adoptions required more performative
 *   justification, while contemporary progressive scholarship presents naskh
 *   arguments with increasing hermeneutical coherence, reducing performative
 *   content.
 *
 * KEY AGENTS:
 *   - Progressive Reform Scholars: Organized institutional actors (institutional/constrained or institutional/mobile depending on context) — primary beneficiaries who gain authority to argue for gender parity reform using Islamic principle
 *   - Communities With Literal Identity Fusion: Distributed powerless agents (powerless/identity_locked) — primary victims whose identity is directly delegitimized; suppression is internalized (cannot exit frame)
 *   - Women Under Literal Legal Structures: Trapped agents (moderate/trapped, powerless/trapped) — secondary victims bearing costs of suppression via legal barriers and community enforcement
 *   - Progressive Scholars in Traditional Institutions: Constrained institutional actors (institutional/constrained) — experience mixed extraction (career risk) and coordination (genuine resolution of textual contradiction)
 *   - Traditional Jurisprudence Institutions: Institutional beneficiaries (institutional/arbitrage) — maintain traditional reading through institutional inertia but experience legitimacy erosion from this reading's diffusion
 *   - State Legislatures Using This Reading: Powerful institutional actors (powerful/arbitrage) — adopt the reading as scaffold to justify gender-parity laws; may co-opt the reading for state consolidation of religious authority
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing contested hermeneutical choice as textual immutability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.78).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.68).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.78).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, snare).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Qur'anic Gender Verses: Progressive Abrogation Reading").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "islamic_jurisprudence/legal_hermeneutics/gender_studies").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, '2b62fe34-58e1-4a09-b1d0-159274351805').
narrative_ontology:cs_kernel_codification('2b62fe34-58e1-4a09-b1d0-159274351805', fixed_text).
narrative_ontology:cs_authority_grounding('2b62fe34-58e1-4a09-b1d0-159274351805', lineage).
narrative_ontology:cs_interpretation_layer_present('2b62fe34-58e1-4a09-b1d0-159274351805').
narrative_ontology:cs_reading_relation('2b62fe34-58e1-4a09-b1d0-159274351805', quranic_gender_verses__literal_hierarchical, coexists_with).
narrative_ontology:cs_reading_relation('2b62fe34-58e1-4a09-b1d0-159274351805', quranic_gender_verses__contextual_egalitarian, influences).
narrative_ontology:cs_axiom('2b62fe34-58e1-4a09-b1d0-159274351805', foundational, naskh_applies_to_gender_law).
narrative_ontology:cs_axiom_status(naskh_applies_to_gender_law, holdable).
narrative_ontology:cs_axiom_grounding('2b62fe34-58e1-4a09-b1d0-159274351805', naskh_applies_to_gender_law, deontological).
narrative_ontology:cs_axiom('2b62fe34-58e1-4a09-b1d0-159274351805', foundational, universal_dignity_principle_abrogates_hierarchy).
narrative_ontology:cs_axiom_status(universal_dignity_principle_abrogates_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('2b62fe34-58e1-4a09-b1d0-159274351805', universal_dignity_principle_abrogates_hierarchy, deontological).
narrative_ontology:cs_reference_frame('2b62fe34-58e1-4a09-b1d0-159274351805', classical_naskh_jurisprudence_applied_to_gender).
narrative_ontology:cs_drift_state('2b62fe34-58e1-4a09-b1d0-159274351805', contemporary_global_discourse_post_2000, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2b62fe34-58e1-4a09-b1d0-159274351805', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, progressive_reform_scholars).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, women_seeking_legal_parity).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, traditional_authority_structures).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, communities_with_literal_identity_fusion).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, scholars_in_institutional_constraints).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMUNITIES WITH LITERAL IDENTITY FUSION (SNARE) — This reading directly delegitimizes the scriptural foundation of their identity and institutional structures. The suppression is internalized: members cannot exit because their identity is constituted through the literal reading. High extractiveness — the reading treats their foundational commitments as superseded, and they bear the cost of identity dissolution without choice.
constraint_indexing:constraint_classification(quranic_gender_verses__progressive_abrogation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: PROGRESSIVE REFORM COALITIONS (ROPE) — Organized agents (reform scholars, women's rights advocates, secular governance movements) see this reading as genuine coordination: resolving the contradiction between egalitarian principles and gender-specific rules. The reading benefits them by providing textual grounds for legal reform. Moderate effective extraction because they have agency and perceive the reading as enabling their goals.
constraint_indexing:constraint_classification(quranic_gender_verses__progressive_abrogation, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: PROGRESSIVE SCHOLARS WITHIN TRADITIONAL INSTITUTIONS (TANGLED ROPE) — Scholars in universities, fatwa councils, or judicial systems who adopt this reading face career constraints: institutional pressure from colleagues, institutional pressure from traditional boards, risk of delegitimization within conservative scholarly circles. But the reading also enables them to claim textual authority for reform — genuine coordination function (resolving contradiction via principle of naskh). Significant extraction (career risk) but not maximal (some institutional paths exist for this reading in some contexts).
constraint_indexing:constraint_classification(quranic_gender_verses__progressive_abrogation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: NATIONAL LEGISLATURES ADOPTING GENDER PARITY LAWS (SCAFFOLD) — State actors using this reading to justify secular gender-parity legislation see it as temporary scaffolding: the reading provides religious legitimacy for laws that would otherwise face institutional resistance from conservative constituencies. Once gender parity becomes institutionalized norm, the religious warrant becomes less critical — the sunset clause is implicit. Low theater because the coordination function (justifying secular reform via Islamic principle) is real, not performative.
constraint_indexing:constraint_classification(quranic_gender_verses__progressive_abrogation, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL ISLAMIC JURISPRUDENCE INSTITUTIONS (PITON) — Classical jurisprudential structures (madhhab schools, fatwa councils with hereditary or conservative authority) maintain traditional gender readings through institutional inertia. The progressive reading creates structural pressure on their authority, but they sustain the traditional reading through theater: citing classical precedent, maintaining formal interpretive protocols, selective citation of naskh principles that preserve gender hierarchy. Theater ratio high because the institutional maintenance of literal reading serves preservation of authority, not resolution of textual contradiction.
constraint_indexing:constraint_classification(quranic_gender_verses__progressive_abrogation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: WOMEN IN COMMUNITIES WITH ENFORCED LITERAL READING (SNARE) — Women subject to legal structures grounded in literal gender-specific verses (guardianship requirements, testimony weight, inheritance rules) experience this reading as pure extraction with suppression via legal barriers and community enforcement. They are trapped between: exit costs (family dissolution, community expulsion, loss of economic security) and the impossibility of seeing their situation as changeable from within the literal frame. Maximum experienced extraction — they bear costs with no exit option and no perceptual frame for alternatives.
constraint_indexing:constraint_classification(quranic_gender_verses__progressive_abrogation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / LITERAL TEXTUAL IMMUTABILITY (MOUNTAIN) — From a civilizational view, one could argue that Qur'anic verses are immutable revealed text, and their surface meaning is unchangeable — gender hierarchy in Qur'an is a structural feature of the revelation itself, not a historical artifact subject to interpretive revision. This reading risks naturalizing a contested hermeneutical choice (how to read naskh, which principles apply) as textual immutability. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(quranic_gender_verses__progressive_abrogation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__progressive_abrogation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quranic_gender_verses__progressive_abrogation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quranic_gender_verses__progressive_abrogation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, TR),
    TR >= 0.70.

:- end_tests(quranic_gender_verses__progressive_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. This reading directly delegates traditional gender-hierarchy structures to 'abrogated law' status, imposing comprehensive normative reversal on communities whose authority and identity rest on literal reading. The reading treats as overturned the entire classical jurisprudential apparatus that grounded gender-specific rules. Beneficiaries (progressive scholars, women gaining legal parity) experience substantial gain, while traditionalist communities and scholars experience loss of hermeneutical ground. The asymmetry is extreme: there is no middle position — either the egalitarian principle abrogates the hierarchy or it does not. Suppression (0.68): High. For identity-locked agents (communities whose identity is fused with literal reading), suppression is internalized — exit would require abandoning identity, not just changing views. For scholars in traditional institutions, suppression is structural (career risk, institutional pressure). For women in communities enforcing literal law, suppression is structural (legal barriers, economic dependency, community sanctions). The reading itself is suppressed by traditionalist institutional structures through authority gatekeeping and delegitimization. Theater ratio (0.55, declining): Moderate and declining. Early progressive scholarship required more performative justification and hermeneutical gymnastics to extend naskh principle to gender domain. Contemporary progressive scholarship (last 20 years) has developed coherent naskh arguments grounded in classical jurisprudential precedent, reducing performative content. The declining trajectory reflects maturation of the reading's hermeneutical infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   Extreme perspectival divergence. Communities with literal identity fusion see this reading as destruction of foundational authority and epistemic violence — a snare that traps them in identity-lock. Progressive reform scholars see it as genuine coordination solving a logical contradiction via principle — a rope enabling their goals. Scholars in traditional institutions see mixed extraction and coordination — tangled_rope. State legislatures see it as temporary scaffolding for reform until gender parity becomes institutionalized norm. Traditional institutions see it as institutional threat maintained by conserving theater around literal reading. Women trapped under literal law see it as pure extraction with no exit option — snare. The analytical observer risks seeing all gender rules as immutably inscribed and immutable — mountain (false summit). The gap reflects that each agent occupies a genuinely different structural position within the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation of d (directionality) for each perspective from beneficiary/victim status and exit options: Progressive scholars are beneficiaries with mobile or arbitrage exit (d ≈ 0.15–0.20, low extraction experience). Identity-locked communities are victims with identity_locked exit (d ≈ 0.89, very high extraction experience). Women in literal-law communities are victims with trapped exit (d ≈ 0.95, maximum extraction experience). Scholars in traditional institutions are both beneficiaries (gain reform authority) and victims (face career cost) with constrained exit (d ≈ 0.55–0.60, moderate extraction experience). State actors are beneficiaries with arbitrage exit (d ≈ 0.05–0.15, low extraction experience). The piton perspective emerges from high theater (0.55) combined with low effective extraction experience for institutions maintaining literal reading through inertia — the theater itself is the institution's response to legitimacy erosion from this reading's diffusion.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED VIA COMMITTER FRAME CLARITY. The mandatrophy—whether this reading is legitimate Islamic hermeneutics, epistemic violence against traditionalist communities, or genuine liberation for women—cannot be resolved within a single framework. The reading is simultaneously all three, depending on which community's authority structure and identity commitments you adopt. The progressive abrogation reading is a coherent Islamic jurisprudential position (naskh principle is classical; its application to gender rules has precedent) AND it constitutes epistemic violence against communities whose identity is fused with literal reading AND it enables women's liberation from legal subjugation. These are not contradictory; they are perspectival truths. The reading does not dissolve the mandatrophy but instantiates it structurally. Each of the three sibling readings (literal_hierarchical, contextual_egalitarian, progressive_abrogation) resolves different aspects of the Qur'anic tension between universalized human dignity and gender-specific legal rules, and each imposes costs on different communities. The mandatrophy is resolved by acknowledging that this is a kernel of genuine civilizational disagreement where no single reading can be universal without epistemic violence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naskh_principle_scope_ambiguity,
    'Does the principle of naskh (abrogation) legitimately apply to gender-specific rules, or do such rules belong to a category of immutable legal commands whose ''abrogation'' would constitute violation of textual integrity?',
    'Hermeneutical genealogy: trace naskh principle application through classical jurisprudence; identify whether classical scholars applied naskh to gender rules and with what textual justification; compare textual warrant for naskh in gender domain vs. other domains (ritual, contract law)',
    'If naskh applies: progressive reading''s core principle is hermeneutically coherent and progressive abrogation is a defensible Islamic jurisprudential position. If naskh does not apply: progressive reading commits hermeneutical violence (illegitimate application of principle) and the reading should reclassify or dissolve. Critical for epistemic authority of the reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naskh_principle_scope_ambiguity, conceptual, 'Whether naskh principle legitimately applies to gender-specific rules').

omega_variable(
    identity_lock_exit_cost_asymmetry,
    'For agents with identity fusion in literal reading (communities, scholars, women raised in traditional frameworks), is the suppression experienced as structural barriers (legal prohibition, economic dependency) or as internalized identity frame that persists even after structural barriers are removed?',
    'Longitudinal case study: track women and scholars who exit literal-reading frameworks; measure whether suppression (self-doubt, internalized constraint, identity dissonance) persists after removal of legal/economic barriers; compare cost-of-exit data across cultural contexts',
    'If suppression is structural: focus remediation on removing legal barriers; the reading''s adoption in secular law is sufficient. If suppression is internalized: the reading alone is insufficient to change lived constraint; requires epistemic work (counter-framing, identity reconstruction, community reintegration) beyond hermeneutical argument.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_exit_cost_asymmetry, empirical, 'Structural vs. internalized suppression in identity-locked agents').

omega_variable(
    reading_specific_vs_tradition_general,
    'Is the progressive abrogation reading grounded in textual interpretation that is genuinely Islamic, or does it require transplanting secular hermeneutical assumptions (progressive history, scientific consensus on gender equality) into Islamic jurisprudence?',
    'Genealogical analysis of reading''s scholarly lineage; identify whether reading''s core arguments (naskh application, principle of egalitarianism) draw from classical Islamic sources or from secular ethical frameworks; assess whether reading could be derived from Islamic commitments alone',
    'If internally coherent to Islamic jurisprudence: reading has hermeneutical authority independent of secular approval; legitimacy is Islamic. If requires secular transplant: reading is hybrid and faces delegitimization from both traditionalists (foreign hermeneutics) and secularists (still rooted in religious authority); classification may shift to lower extractiveness if the reading''s authority base erodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_specific_vs_tradition_general, conceptual, 'Whether reading is internally Islamic or requires secular hermeneutical framework').

omega_variable(
    institutional_power_consolidation_vs_epistemic_liberation,
    'Does adoption of progressive abrogation reading by state legislatures and institutional actors constitute genuine epistemic liberation for women and egalitarian reform, or does it constitute co-optation of the reading to consolidate state power over religious interpretation while leaving underlying patriarchal structures intact?',
    'Institutional analysis: compare states that adopt progressive reading in law with actual enforcement of gender parity; measure whether legal parity translates to lived equality; assess whether adoption of reading coincides with state control over religious authority or institutional pluralism',
    'If genuine liberation: the reading''s extractiveness is overstated; benefits for women are real and the reading enables structural reform. If co-optation: the reading becomes a tool for state consolidation of religious authority and may entrench patriarchy at a different institutional level; extractiveness persists in new form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_power_consolidation_vs_epistemic_liberation, empirical, 'Whether institutional adoption of reading delivers epistemic liberation or constitutes co-optation').

omega_variable(
    committer_kernel_reading_declaration,
    'This constraint is a READING of the contested kernel ''quranic_gender_verses''. What is the structural relationship between this reading and its sibling readings (''literal_hierarchical'', ''contextual_egalitarian'')?',
    'This omega documents the committer-frame structure per Rule 2 (route committer structure to omegas). See cs_structure.reading_relations for formal relationships. This omega instantiates the authoring discipline that one constraint = one reading = one clean epsilon-invariant classification.',
    'This reading does not claim to be the only hermeneutically valid reading, nor does it claim that alternative readings are logically incoherent. It claims that THIS reading instantiates a specific constraint structure (high extractiveness + suppression + snare from powerless perspective) and that sibling readings instantiate different constraint structures. The kernel itself is contested; this story models only this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_declaration, conceptual, 'Kernel reading declaration: progressive_abrogation is one of three sibling readings; see cs_structure for relations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qur_prog_abr_tr_t0, quranic_gender_verses__progressive_abrogation, theater_ratio, 0, 0.65).
narrative_ontology:measurement(qur_prog_abr_tr_t20, quranic_gender_verses__progressive_abrogation, theater_ratio, 20, 0.58).
narrative_ontology:measurement(qur_prog_abr_tr_t40, quranic_gender_verses__progressive_abrogation, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(qur_prog_abr_be_t0, quranic_gender_verses__progressive_abrogation, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(qur_prog_abr_be_t20, quranic_gender_verses__progressive_abrogation, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(qur_prog_abr_be_t40, quranic_gender_verses__progressive_abrogation, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(qur_prog_abr_su_t0, quranic_gender_verses__progressive_abrogation, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(qur_prog_abr_su_t20, quranic_gender_verses__progressive_abrogation, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(qur_prog_abr_su_t40, quranic_gender_verses__progressive_abrogation, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__progressive_abrogation, identity_coordination).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, islamic_legal_authority_structures).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, gender_parity_legislation_in_muslim_states).

% DUAL FORMULATION NOTE:
% Three distinct constraint stories model three readings of the quranic_gender_verses kernel: progressive_abrogation (this story, ε=0.78, snare from powerless perspective); literal_hierarchical (ε=0.35–0.45, rope/tangled_rope from different perspectives); contextual_egalitarian (ε=0.25–0.35, rope/scaffold). Each reading has its own epsilon because the observable used to evaluate each differs — the naskh principle's validity, the hermeneutical mechanism, and the normative consequence. Linking all three via network.affects_constraints preserves the kernel structure: all three are live readings of one contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__progressive_abrogation, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
