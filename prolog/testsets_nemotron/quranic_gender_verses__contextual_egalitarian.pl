% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__contextual_egalitarian
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: quranic_gender_verses__contextual_egalitarian
 *   human_readable: Contextual-Egalitarian Reading of Qur'anic Gender Verses (Maqasid-Based Reinterpretation)
 *   domain: islamic_jurisprudence/legal_hermeneutics/gender_studies
 *
 * SUMMARY:
 *   This constraint instantiates the contextual-egalitarian reading of
 *   Qur'anic gender verses (4:11 inheritance, 2:282 testimony, 4:34 marital
 *   authority). It treats these verses as historically situated progressive
 *   steps within 7th-century Arabia that require reinterpretation under
 *   overarching Qur'anic equity principles (maqasid al-shari'a). The reading
 *   is championed by reformist scholars (e.g., Fazlur Rahman, Amina Wadud,
 *   Abdullahi An-Na'im) and rights-based NGOs (Musawah, Sisters in Islam). It
 *   operates as a tangled rope: it coordinates interpretive communities
 *   around a shared hermeneutic while extracting interpretive authority and
 *   material resources from patriarchal elites and traditional courts. The
 *   constraint requires active enforcement through scholarly debate,
 *   institutional advocacy, and legal reform campaigns to sustain its
 *   coherence against competing readings.
 *
 * KEY AGENTS:
 *   - reformist_scholars: Primary agenda-setters (institutional/biographical/constrained) — produce and legitimize the reinterpretive methodology
 *   - rights_based_ngos: Secondary beneficiaries (organized/biographical/mobile) — operationalize the reading into legal advocacy and community mobilization
 *   - muslim_women_in_egalitarian_communities: Primary beneficiaries (powerless→moderate/biographical/constrained) — gain structural claims to equal inheritance/testimony; exit victim set of literal reading
 *   - patriarchal_elites: Primary victims (powerful→organized/generational/constrained) — lose discretionary interpretive authority and material control over gendered rights
 *   - traditional_qadi_courts: Secondary victims (institutional/generational/trapped) — lose institutional monopoly on gendered legal interpretation
 *   - conservative_ulama_networks: Tertiary victims (organized/generational/identity_locked) — lose epistemic authority and communal legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__contextual_egalitarian, 0.42).
domain_priors:suppression_score(quranic_gender_verses__contextual_egalitarian, 0.28).
domain_priors:theater_ratio(quranic_gender_verses__contextual_egalitarian, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, extractiveness, 0.42).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__contextual_egalitarian, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__contextual_egalitarian, "Contextual-Egalitarian Reading of Qur'anic Gender Verses (Maqasid-Based Reinterpretation)").
narrative_ontology:topic_domain(quranic_gender_verses__contextual_egalitarian, "islamic_jurisprudence/legal_hermeneutics/gender_studies").

domain_priors:requires_active_enforcement(quranic_gender_verses__contextual_egalitarian).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__contextual_egalitarian, '536e8bbe-76b6-49be-9ce9-e3fe76adc58c').
narrative_ontology:cs_kernel_codification('536e8bbe-76b6-49be-9ce9-e3fe76adc58c', fixed_text).
narrative_ontology:cs_authority_grounding('536e8bbe-76b6-49be-9ce9-e3fe76adc58c', lineage).
narrative_ontology:cs_interpretation_layer_present('536e8bbe-76b6-49be-9ce9-e3fe76adc58c').
narrative_ontology:cs_reading_relation('536e8bbe-76b6-49be-9ce9-e3fe76adc58c', quranic_gender_verses__literal_hierarchical, influences).
narrative_ontology:cs_reading_relation('536e8bbe-76b6-49be-9ce9-e3fe76adc58c', quranic_gender_verses__progressive_abrogation, coexists_with).
narrative_ontology:cs_axiom('536e8bbe-76b6-49be-9ce9-e3fe76adc58c', foundational, verses_are_historically_situated_progressive_steps).
narrative_ontology:cs_axiom_status(verses_are_historically_situated_progressive_steps, holdable).
narrative_ontology:cs_axiom_grounding('536e8bbe-76b6-49be-9ce9-e3fe76adc58c', verses_are_historically_situated_progressive_steps, empirically_contingent).
narrative_ontology:cs_axiom('536e8bbe-76b6-49be-9ce9-e3fe76adc58c', foundational, maqasid_equity_principles_override_specific_verses).
narrative_ontology:cs_axiom_status(maqasid_equity_principles_override_specific_verses, holdable).
narrative_ontology:cs_axiom_grounding('536e8bbe-76b6-49be-9ce9-e3fe76adc58c', maqasid_equity_principles_override_specific_verses, deontological).
narrative_ontology:cs_reference_frame('536e8bbe-76b6-49be-9ce9-e3fe76adc58c', classical_fiqh_gender_framework).
narrative_ontology:cs_drift_state('536e8bbe-76b6-49be-9ce9-e3fe76adc58c', post_colonial_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('536e8bbe-76b6-49be-9ce9-e3fe76adc58c', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, reformist_scholars).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, rights_based_ngos).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, muslim_women_in_egalitarian_communities).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, patriarchal_elites).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, traditional_qadi_courts).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, conservative_ulama_networks).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, maqasid_equity_principle).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, historical_contextualization_method).
narrative_ontology:constraint_vindicates(quranic_gender_verses__contextual_egalitarian, universal_human_dignity_quranic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and legitimize the maqasid-based historical-contextual methodology through academic publications, fatwas, and institutional positions (e.g., Al-Azhar reformist wing, International Union of Muslim Scholars progressive faction). Their interpretive authority depends on recognition within the Islamic scholarly ecosystem; exit means losing scholarly credibility and institutional access. They gain funding, platform access, and discipleship networks from the reading's adoption.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, reformist_scholars, agenda_setter,
    institutional, biographical, constrained, global).

% Organizations like Musawah, Sisters in Islam, Karamah operationalize the reading into legal advocacy, community education, and international human rights engagement. They gain grant funding, policy access, and mobilization capacity. Their exit is mobile — they can pivot to secular rights frameworks or other religious traditions — but they are structurally positioned as the reading's organizational infrastructure.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, rights_based_ngos, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, rights_based_ngos, agenda_setter).

% Women in communities where the reading has gained legal or social traction (e.g., post-2004 Morocco, parts of Malaysia/Indonesia, diaspora reformist communities). They gain structural claims to equal inheritance shares, equal testimony weight, and marital autonomy. Their exit is constrained: they remain embedded in family/community structures that may resist the reading; state recognition of reformed interpretations is partial and reversible. They are the primary measure of whether the constraint delivers on its coordination promise.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, muslim_women_in_egalitarian_communities, beneficiary,
    moderate, biographical, constrained, regional).

% Tribal leaders, wealthy merchant families, political elites whose authority and material control (inheritance distribution, marriage guardianship, testimony weight in commercial disputes) depend on the literal reading. They lose discretionary power as the reading gains legal recognition. Their exit is constrained: they can resist through political influence, fund counter-institutions, or relocate capital, but their identity and authority are structurally bound to the patriarchal interpretation.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, patriarchal_elites, payer,
    powerful, generational, constrained, regional).

% State-appointed religious courts (e.g., in Saudi Arabia, UAE, Pakistan, Malaysia) that hold monopoly on family law adjudication. They lose institutional jurisdiction and interpretive monopoly as legislatures enact reformed codes or constitutional courts invalidate gender-differentiated rules. Their exit is trapped: they are state institutions that cannot easily dissolve or transform; judges' careers and institutional survival depend on the existing framework. They resist through procedural obstruction, narrow construction of reforms, and fatwa-shopping.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, traditional_qadi_courts, payer,
    institutional, generational, trapped, national).

% Transnational networks of traditionalist scholars (e.g., Deobandi, Salafi, traditional Al-Azhar circles) whose epistemic authority, communal legitimacy, and institutional funding rest on defending the literal reading as the only valid interpretation. They are identity-locked: their scholarly identity is fused to the claim that the gender verses are unambiguous divine ordinance; adopting the contextual reading would dissolve their self-concept and communal standing. They are excluded from the reformist conversation by design — their dissent is treated as illegitimate by the reading's proponents.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, conservative_ulama_networks, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__contextual_egalitarian, conservative_ulama_networks, excluded).

% UN bodies, international human rights NGOs, secular feminist scholars who engage the reading as a strategic ally or a problematic compromise. They neither collect nor pay within the Islamic interpretive economy but shape the external legitimacy environment. Their analytical seat sees the full structural field across all three readings.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__contextual_egalitarian, secular_feminist_observers, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of applying 7th-century Qur'anic gender verses to modern Muslim societies without abandoning textual authority or fracturing communal cohesion. Provides a hermeneutic method (historical contextualization + maqasid principles) that allows Muslims to affirm the Qur'an's divine origin while reaching egalitarian legal outcomes.
% TRANSFER_FUNCTION: Moves interpretive authority, institutional resources (court jurisdiction, fatwa-issuing capacity, educational curricula), and material gendered rights (inheritance shares, testimony weight, marital authority) from patriarchal elites and traditional courts to reformist scholars, rights-based NGOs, and Muslim women in egalitarian communities.
% ABSENT_VOICES: Women in conservative communities who cannot access the reading's protections (trapped by family/community enforcement); queer and trans Muslims whose gender/sexuality falls outside the binary framework of all three readings; ex-Muslims who reject the kernel entirely but remain subject to its legal effects in Muslim-majority jurisdictions. These voices are excluded by the kernel's own terms — the debate occurs within the commitment to Qur'anic authority.
% DISAPPEARANCE_RATIONALE: If the contextual-egalitarian reading vanished overnight, reformist scholars and NGOs would lose their primary Islamic-framework legitimacy; women in reformed jurisdictions would lose the hermeneutic shield protecting their legal gains; patriarchal elites and traditional courts would regain unchallenged interpretive monopoly; the intra-community conflict would not disappear but would shift to a binary between literal_hierarchical and progressive_abrogation readings. The Muslim world's legal landscape would rearrange significantly.
% FOUNDING_PROBLEM: How to reconcile the Qur'an's gender-differentiated verses (revealed in 7th-century Arabia) with the overarching Qur'anic principles of justice, equity, and human dignity (maqasid) in modern contexts — without rejecting the text's divine authority or conceding that Islam is inherently patriarchal.
% FOUNDING_PROBLEM_CORROBORATION: Classical usul scholars (al-Shatibi, Ibn Ashur) corroborate the maqasid methodology as internal to the tradition. Modern reformist scholars (Rahman, Wadud, An-Na'im) attest the problem remains live. Traditionalist ulama (Ibn Baz, al-Albani, Deobandi fatwa councils) contest the problem's framing — they deny a tension exists, claiming the literal reading IS the equity principle. No consensus exists outside the benefiting parties; the dispute is structural.
narrative_ontology:disappearance_verdict(quranic_gender_verses__contextual_egalitarian, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__contextual_egalitarian, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__contextual_egalitarian, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(quranic_gender_verses__contextual_egalitarian, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__contextual_egalitarian, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__contextual_egalitarian_tests).
:- end_tests(quranic_gender_verses__contextual_egalitarian_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the reading extracts interpretive authority and institutional resources from patriarchal structures, redirecting them toward egalitarian outcomes. The extraction is not zero because the reading must actively displace entrenched interpretive monopolies — it does not merely describe a natural equilibrium. Suppression (0.28) is low-to-moderate: the constraint's persistence depends on scholarly persuasion and legal advocacy, not coercion; alternatives (literal reading) remain openly available. Theater ratio (0.35) reflects genuine hermeneutic work mixed with performative alignment to modern rights discourse. Accessibility collapse (0.45) is moderate: the historical-contextual method opens interpretive space but requires specialized training. Resistance (0.55) is significant: the reading faces organized opposition from traditionalist institutions and identity-locked scholars.
 *
 * PERSPECTIVAL GAP:
 *   From the reformist scholar seat, the constraint appears as a rope — genuine coordination solving the problem of applying 7th-century texts to modern equity. From the patriarchal elite seat, it appears as a snare — extraction of their hereditary interpretive privilege under cover of 'reform.' From the Muslim women seat, it appears as a scaffold — temporary support for rights claims that should eventually become self-sustaining. The engine computes these divergences from the structural data; the claimed_type (tangled_rope) reflects the structural reality that both coordination and asymmetric extraction are present.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars and rights-based NGOs are structural beneficiaries (d ~ 0.2): they gain interpretive authority, funding, and institutional position. Muslim women in egalitarian communities are primary beneficiaries (d ~ 0.15): they exit the victim set of the literal reading and gain structural claims. Patriarchal elites and traditional courts are structural targets (d ~ 0.85): they lose discretionary power and material control. Conservative ulama are identity-locked targets (d ~ 0.9): their professional identity is fused to the literal reading, making exit cognitively costly. The directionality derivation follows from beneficiary/victim declarations + exit modulation: mobile/arbitrage exit for NGOs dampens d; identity_locked for conservative ulama amplifies d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (applying Qur'anic gender verses to modern contexts without abandoning textual authority) remains live — hence founding_problem_status = contested. The reading has not resolved into a stable coordination equilibrium because the literal_hierarchical reading retains institutional power in many jurisdictions, and the progressive_abrogation reading offers a competing methodological path. Mandatrophy is not resolved: the constraint's hermeneutic machinery remains actively maintained because the interpretive dispute is unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_structural_delta,
    'How does the contextual-egalitarian reading''s structural delta (moderate extractiveness; reformist scholars/NGOs gain authority; women exit victim set; patriarchal elites/courts lose power) differ from the sibling readings'' deltas, and does this delta stabilize or intensify intra-community conflict?',
    'Comparative institutional analysis of reform outcomes in jurisdictions where each reading has gained legal traction (e.g., Morocco''s 2004 Mudawwana reform vs. Saudi personal status law vs. Indonesian Constitutional Court rulings). Track interpretive authority shifts, resource flows, and conflict metrics over 20-year windows.',
    'If the delta stabilizes (conflict diminishes as egalitarian interpretation becomes settled law), the constraint trends toward rope. If conflict intensifies (backlash, polarization, competing fatwas), the constraint remains tangled_rope or trends toward snare from the patriarchal seat. The engine''s per-seat classification will reflect this divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_structural_delta, empirical, 'Whether the contextual-egalitarian reading''s structural displacement of patriarchal authority produces stable coordination or sustained contestation').

omega_variable(
    hermeneutic_method_legitimacy,
    'Is the maqasid-based historical-contextual method epistemically internal to the Islamic legal tradition (a legitimate development of usul al-fiqh) or an external imposition (secular liberal norms dressed in Islamic vocabulary)?',
    'Genealogical analysis of the method''s provenance: trace its conceptual lineage through classical usul works (Shatibi, Ibn Ashur) vs. modern reformist appropriations. Assess whether the method''s criteria for ''overarching principles'' are derivable from within the tradition or require extra-traditional normative commitments.',
    'If internal, the reading''s coordination function is genuine (rope-like) and its extractiveness is the cost of internal reform. If external, the reading''s coordination story is cover for ideological imposition (snare-like from traditionalist seats). This ambiguity directly bears on the claimed_type and the engine''s Boltzmann analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hermeneutic_method_legitimacy, conceptual, 'Whether the contextual-egalitarian hermeneutic is an internal development or external imposition — the core legitimacy dispute').

omega_variable(
    women_exit_victim_set_completeness,
    'Do Muslim women in egalitarian communities fully exit the victim set, or do they remain partially captured by residual patriarchal structures (e.g., family pressure, community enforcement, state non-recognition of reformed interpretations)?',
    'Longitudinal ethnographic and legal-pluralism studies in communities adopting the reading: measure women''s actual exercise of equal inheritance/testimony rights vs. formal legal recognition. Track gap between statutory reform and lived practice.',
    'If exit is incomplete, the constraint''s effective extractiveness from the women''s seat is higher than the authored 0.42 suggests — residual extraction persists. This would shift the per-seat classification toward snare for women in non-reformed jurisdictions, creating a spatial-scope divergence the engine computes from scope + exit_options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_exit_victim_set_completeness, empirical, 'Completeness of women''s exit from victim set under the contextual-egalitarian reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__contextual_egalitarian, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quranic_gender_verses__contextual_egalitarian_tr_t1970, quranic_gender_verses__contextual_egalitarian, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(quranic_gender_verses__contextual_egalitarian_tr_t1990, quranic_gender_verses__contextual_egalitarian, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(quranic_gender_verses__contextual_egalitarian_tr_t2005, quranic_gender_verses__contextual_egalitarian, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(quranic_gender_verses__contextual_egalitarian_tr_t2015, quranic_gender_verses__contextual_egalitarian, theater_ratio, 2015, 0.34).
narrative_ontology:measurement(quranic_gender_verses__contextual_egalitarian_tr_t2025, quranic_gender_verses__contextual_egalitarian, theater_ratio, 2025, 0.35).

% Extraction over time
narrative_ontology:measurement(quranic_gender_verses__contextual_egalitarian_be_t1970, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(quranic_gender_verses__contextual_egalitarian_be_t1990, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(quranic_gender_verses__contextual_egalitarian_be_t2005, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(quranic_gender_verses__contextual_egalitarian_be_t2015, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 2015, 0.43).
narrative_ontology:measurement(quranic_gender_verses__contextual_egalitarian_be_t2025, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(quranic_gender_verses__contextual_egalitarian_su_t1970, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(quranic_gender_verses__contextual_egalitarian_su_t1990, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 1990, 0.18).
narrative_ontology:measurement(quranic_gender_verses__contextual_egalitarian_su_t2005, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 2005, 0.25).
narrative_ontology:measurement(quranic_gender_verses__contextual_egalitarian_su_t2015, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 2015, 0.28).
narrative_ontology:measurement(quranic_gender_verses__contextual_egalitarian_su_t2025, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 2025, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__contextual_egalitarian, identity_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__contextual_egalitarian, 0.08).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__progressive_abrogation).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, muslim_personal_status_law_reform).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, womens_rights_advocacy_islamic_frameworks).

% DUAL FORMULATION NOTE:
% The quranic_gender_verses kernel decomposes into three constraint stories: contextual_egalitarian (this story, tangled_rope), literal_hierarchical (mountain claim, likely false_summit_mountain candidate), and progressive_abrogation (scaffold/tangled_rope). The contextual_egalitarian reading influences both siblings: it creates downstream legitimacy pressure on literal_hierarchical by demonstrating internal-tradition grounds for egalitarian outcomes, and it competes with progressive_abrogation for the reformist methodological space. All three share the same referent verses but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__contextual_egalitarian, institutional, 0.15).
constraint_indexing:directionality_override(quranic_gender_verses__contextual_egalitarian, organized, 0.2).
constraint_indexing:directionality_override(quranic_gender_verses__contextual_egalitarian, powerless, 0.1).
constraint_indexing:directionality_override(quranic_gender_verses__contextual_egalitarian, powerful, 0.85).
constraint_indexing:directionality_override(quranic_gender_verses__contextual_egalitarian, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
