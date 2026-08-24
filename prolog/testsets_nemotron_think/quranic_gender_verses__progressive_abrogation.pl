% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__progressive_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: quranic_gender_verses__progressive_abrogation
 *   human_readable: Progressive Abrogation Reading of Qur'anic Gender Verses
 *   domain: religious/hermeneutic/legal
 *
 * SUMMARY:
 *   The progressive abrogation reading argues that Qur'an 49:13 ('O mankind,
 *   indeed We have created you from male and female...') establishes a
 *   universal human dignity principle that, through the classical principle
 *   of naskh (abrogation), supersedes earlier gender-differentiated verses on
 *   inheritance (4:11), testimony (2:282), and marital guardianship (4:34).
 *   This reading does not merely reinterpret; it declares the earlier verses
 *   legally inoperative. As a constraint, it operates within Islamic legal
 *   hermeneutics: it coordinates by providing a single decisive method for
 *   resolving intra-textual conflict, but extracts asymmetrically by
 *   delegitimizing the traditional scholarly authorities whose interpretive
 *   monopoly rests on the classical hierarchy of verses. The constraint
 *   requires active enforcement — institutional adoption by courts,
 *   legislatures, and fatwa councils — to have legal effect. Its
 *   extractiveness has risen sharply since the 1990s as international human
 *   rights norms and domestic women's movements have pressed for
 *   codification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.82).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.78).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.82).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Progressive Abrogation Reading of Qur'anic Gender Verses").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "religious/hermeneutic/legal").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, '39717c10-6209-416e-b57c-60ea40f995fb').
narrative_ontology:cs_kernel_codification('39717c10-6209-416e-b57c-60ea40f995fb', fixed_text).
narrative_ontology:cs_authority_grounding('39717c10-6209-416e-b57c-60ea40f995fb', lineage).
narrative_ontology:cs_interpretation_layer_present('39717c10-6209-416e-b57c-60ea40f995fb').
narrative_ontology:cs_reading_relation('39717c10-6209-416e-b57c-60ea40f995fb', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('39717c10-6209-416e-b57c-60ea40f995fb', quranic_gender_verses__contextual_egalitarian, coexists_with).
narrative_ontology:cs_axiom('39717c10-6209-416e-b57c-60ea40f995fb', foundational, later_egalitarian_verses_abrogate_earlier_gender_rules).
narrative_ontology:cs_axiom_status(later_egalitarian_verses_abrogate_earlier_gender_rules, holdable).
narrative_ontology:cs_axiom_grounding('39717c10-6209-416e-b57c-60ea40f995fb', later_egalitarian_verses_abrogate_earlier_gender_rules, conventional).
narrative_ontology:cs_axiom('39717c10-6209-416e-b57c-60ea40f995fb', secondary, women_attain_full_legal_parity_through_abrogation).
narrative_ontology:cs_axiom_status(women_attain_full_legal_parity_through_abrogation, holdable).
narrative_ontology:cs_axiom_grounding('39717c10-6209-416e-b57c-60ea40f995fb', women_attain_full_legal_parity_through_abrogation, deontological).
narrative_ontology:cs_reference_frame('39717c10-6209-416e-b57c-60ea40f995fb', classical_naskh_principle).
narrative_ontology:cs_drift_state('39717c10-6209-416e-b57c-60ea40f995fb', contemporary_gender_egalitarian_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('39717c10-6209-416e-b57c-60ea40f995fb', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, women_seeking_legal_parity).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, progressive_islamic_scholars).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, traditional_scholarly_authorities).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, literalist_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, state_legal_institutions).
narrative_ontology:constraint_vindicates(quranic_gender_verses__progressive_abrogation, universal_human_dignity_quran_49_13).
narrative_ontology:constraint_vindicates(quranic_gender_verses__progressive_abrogation, naskh_as_progressive_hermeneutic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advance the naskh-based reading that later egalitarian verses abrogate earlier gender-specific rules. They publish, teach, and lobby for legal reform within Muslim-majority states and diaspora institutions. Their careers depend on institutional recognition; adopting this reading risks excommunication from traditional seminaries and loss of scholarly authority, but provides access to international human rights networks and reformist funding.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, progressive_islamic_scholars, agenda_setter,
    organized, generational, constrained, global).

% Gain full legal parity in inheritance, testimony, marriage, and guardianship if this reading becomes authoritative. Their exit options are limited by family, community, and state enforcement of existing gender-differentiated laws; migration is possible but costly. They bear the social costs of being labeled 'westernized' or 'un-Islamic' while advocating for this reading.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, women_seeking_legal_parity, beneficiary,
    moderate, biographical, constrained, global).

% Hold authority through madrasa networks, state religious appointments, and fatwa councils. Their legitimacy derives from mastery of the classical interpretive tradition which treats gender-differentiated verses as timeless. This reading delegitimizes their core epistemic claim; they cannot exit without abandoning their professional identity and the institutional structures that sustain them. They actively suppress this reading through curricular control, fatwa issuance, and state lobbying.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, traditional_scholarly_authorities, payer,
    institutional, generational, identity_locked, global).

% Communities whose collective identity is bound to the literal-hierarchical reading of gender verses. They experience this reading as epistemic violence — an attack on the divine ordinance that structures their family life, social order, and self-understanding. They are excluded from the reformist conversation but bear the cost of having their normative world delegitimized. Exit means communal disintegration; they resist through social enforcement, alternative education, and political mobilization.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, literalist_communities, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__progressive_abrogation, literalist_communities, excluded).

% Family law courts, ministries of religious affairs, and constitutional bodies in Muslim-majority states. They administer the current gender-differentiated legal framework. Adopting this reading would require comprehensive legal reform, provoking domestic backlash and international scrutiny. They pay the political cost of either suppressing reform (legitimacy deficit) or implementing it (stability risk). Their exit is constrained by sovereignty, international treaties, and domestic power balances.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, state_legal_institutions, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__progressive_abrogation, state_legal_institutions, payer).

% UN treaty bodies, CEDAW committee, and international NGOs monitor compliance with gender equality norms. They cite this reading as evidence that Islamic law can accommodate full legal parity. They neither collect nor pay within the constraint but shape the external legitimacy landscape for state actors and scholars.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the intra-textual tension between earlier gender-specific verses (4:11 inheritance, 2:282 testimony, 4:34 guardianship) and later universal dignity verses (49:13) by providing a hermeneutic method (naskh) that declares the later verses legally superseding. This coordinates interpretive practice across the legal system without requiring verse-by-verse reconciliation.
% TRANSFER_FUNCTION: Transfers interpretive authority and legal legitimacy from traditional scholarly hierarchies (who monopolize classical tafsir and fiqh) to progressive scholars and rights-based advocates. Transfers legal entitlements from men (under guardianship/inheritance/testimony rules) to women (full parity). Transfers the cost of epistemic dislocation onto literalist communities and traditional authorities.
% ABSENT_VOICES: Classical tafsir authorities (historical) cannot speak; their silence is filled by living traditional scholars claiming to represent them. Women in conservative rural communities who may not desire 'legal parity' as defined by urban reformists are rarely consulted. Minority Muslim communities in non-Muslim-majority states who use gender-differentiated personal law as identity preservation are excluded from the global reform discourse.
% DISAPPEARANCE_RATIONALE: If the naskh-based progressive abrogation reading vanished overnight, the default interpretive framework in most institutional settings would revert to literal-hierarchical or contextual-egalitarian readings. Family law codes in multiple countries would lose their primary Islamic legal justification for gender equality reforms. Women's rights organizations would lose a key theological resource. Traditional authorities would regain unchallenged epistemic dominance. The global Islamic legal discourse would restructure around the remaining two readings.
% FOUNDING_PROBLEM: The classical Islamic legal tradition developed gender-differentiated rules (inheritance shares, testimony weight, male guardianship) from specific Qur'anic verses, treating them as timeless divine ordinances. This created a structural tension with the Qur'an's own universal dignity principle (49:13) and with modern human rights norms that Muslim states have ratified. The founding problem was: how to resolve this intra-Qur'anic and intra-normative tension without abandoning the text's authority?
% FOUNDING_PROBLEM_CORROBORATION: The tension between verse-specific rules and universal principles is acknowledged by both traditional scholars (who resolve it via hierarchical interpretation) and progressive scholars (who resolve it via naskh or maqasid). Classical usul al-fiqh texts document the naskh principle but restrict its application; modern reformists like Fazlur Rahman and Abdullahi An-Na'im argue the classical restriction was itself a historical choice, not a textual necessity. No consensus exists on whether the founding problem is 'solved' by this reading.
narrative_ontology:disappearance_verdict(quranic_gender_verses__progressive_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__progressive_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__progressive_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quranic_gender_verses__progressive_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__progressive_abrogation, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__progressive_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__progressive_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is very high because the constraint's adoption comprehensively reverses the legal entitlements of men under classical fiqh and the epistemic authority of the scholars who maintain that system. Suppression (0.78) is high because traditional authorities actively suppress this reading through curricular control, fatwa councils, and state lobbying — the constraint persists only where enforcement (institutional adoption) overcomes this suppression. Theater ratio (0.35) is moderate: the naskh principle is genuinely classical, but its application to gender verses is a modern innovation; the performance of 'classical method' masks the novelty of the conclusion. Accessibility collapse (0.72) is high because once the abrogation logic is accepted, the alternative (classical hierarchy) becomes legally incoherent within that framework. Resistance (0.85) is very high from traditional authorities and literalist communities whose identity and authority are bound to the classical reading.
 *
 * PERSPECTIVAL GAP:
 *   From the progressive scholar seat, this constraint is a rope (genuine coordination resolving intra-Qur'anic tension via classical method). From the traditional authority seat, it is a snare (pure extraction delegitimizing their epistemic monopoly under cover of a hermeneutic principle they recognize but restrict). From the women's seat, it is a scaffold (transitional coordination toward parity, though no sunset clause exists). The engine computes this divergence from the structural data: same constraint, different directionalities, different effective extractions.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive scholars are agenda_setters with constrained exit (institutional recognition vs. traditional ostracism) — they derive low d (beneficiary direction) from advancing the reading. Women seeking parity are beneficiaries with constrained exit (family/community/state enforcement of existing laws) — they receive the legal transfer. Traditional scholarly authorities are payers with identity_locked exit (their professional self-concept is fused to the classical interpretive tradition) — they bear maximal extraction. Literalist communities are payers/excluded with identity_locked exit (communal identity fused to literal reading) — they bear epistemic violence costs. State institutions are dual agenda_setter/payer with constrained exit (sovereignty/international treaties) — they administer the constraint and pay political costs. International bodies are observers with analytical exit — they shape legitimacy without direct stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The classical naskh principle was developed to resolve specific historical contradictions (e.g., gradual prohibition of alcohol), not as a universal egalitarian override. Its extension to gender verses is a modern repurposing. The constraint's persistence depends on active enforcement (institutional adoption); without it, the classical hierarchy reasserts. This is not mandatrophy — the founding problem (intra-Qur'anic tension + human rights norms) remains live and contested. The constraint is not inertial; it is actively contested and actively enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the progressive_abrogation reading a distinct constraint from its sibling readings, or a rhetorical stance within a single interpretive practice?',
    'Test whether the three readings produce different legal outcomes for the same cases (inheritance division, testimony weight, guardianship authority) under the same institutional conditions. If outcomes diverge, they are distinct constraints.',
    'If distinct constraints, each requires its own ε, stakeholders, and classification. If a single practice, the kernel frame is misapplied and the three should be modeled as perspectival variants of one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the kernel decomposition into three constraint stories is structurally warranted or an analytical imposition.').

omega_variable(
    naskh_scope_ambiguity,
    'Does the classical naskh principle legitimately extend to abrogate gender-differentiated verses, or is this a modern innovation masquerading as tradition?',
    'Survey classical usul al-fiqh texts for precedents of naskh applied to social-status verses (vs. ritual/ritual-prohibition verses). Assess whether the verse-chronology required (49:13 revealed after 4:34) is historically established.',
    'If naskh was classically restricted to ritual/legal-prohibition verses, this reading''s coordination claim (classical method) is theater; extractiveness is higher than measured. If naskh had broad classical scope, the coordination function is genuine and extractiveness lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naskh_scope_ambiguity, empirical, 'Whether the hermeneutic method claimed by this reading is genuinely classical or a modern retrofit.').

omega_variable(
    epistemic_violence_measurement,
    'How to measure the epistemic violence inflicted on literalist communities whose identity is constituted by the literal-hierarchical reading?',
    'Longitudinal ethnographic study of communities undergoing legal reform driven by this reading: track identity disruption, communal cohesion loss, and intergenerational transmission failure.',
    'If epistemic violence is structurally significant and irreducible, the constraint''s extraction from literalist communities is higher than current metrics capture (which focus on legal entitlements). This could shift classification toward snare for the payer seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_violence_measurement, conceptual, 'Whether and how to quantify identity-constitutive harm as extraction.').

omega_variable(
    state_adoption_as_enforcement,
    'Is state codification of this reading ''active enforcement'' of the constraint, or merely the constraint''s natural institutionalization?',
    'Compare jurisdictions where this reading was adopted via legislative reform (top-down) vs. scholarly consensus (bottom-up). Measure whether the constraint persists without state enforcement.',
    'If the constraint collapses without state enforcement, it is a scaffold masquerading as tangled_rope (coordination dependent on external power). If it self-sustains via scholarly networks, tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_adoption_as_enforcement, empirical, 'Whether the constraint''s enforcement mechanism is endogenous or state-dependent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_tr_t1950, quranic_gender_verses__progressive_abrogation, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_tr_t1970, quranic_gender_verses__progressive_abrogation, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_tr_t1990, quranic_gender_verses__progressive_abrogation, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_tr_t2005, quranic_gender_verses__progressive_abrogation, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_tr_t2015, quranic_gender_verses__progressive_abrogation, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_tr_t2025, quranic_gender_verses__progressive_abrogation, theater_ratio, 2025, 0.35).

% Extraction over time
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_be_t1950, quranic_gender_verses__progressive_abrogation, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_be_t1970, quranic_gender_verses__progressive_abrogation, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_be_t1990, quranic_gender_verses__progressive_abrogation, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_be_t2005, quranic_gender_verses__progressive_abrogation, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_be_t2015, quranic_gender_verses__progressive_abrogation, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_be_t2025, quranic_gender_verses__progressive_abrogation, base_extractiveness, 2025, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_su_t1950, quranic_gender_verses__progressive_abrogation, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_su_t1970, quranic_gender_verses__progressive_abrogation, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_su_t1990, quranic_gender_verses__progressive_abrogation, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_su_t2005, quranic_gender_verses__progressive_abrogation, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_su_t2015, quranic_gender_verses__progressive_abrogation, suppression_requirement, 2015, 0.74).
narrative_ontology:measurement(quranic_gender_verses__progressive_abrogation_su_t2025, quranic_gender_verses__progressive_abrogation, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__progressive_abrogation, identity_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__progressive_abrogation, 0.08).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__contextual_egalitarian).

% DUAL FORMULATION NOTE:
% This constraint story is one member of the quranic_gender_verses constraint family. The three readings (progressive_abrogation, literal_hierarchical, contextual_egalitarian) are structurally distinct constraints linked by shared textual referent but different hermeneutic operations, beneficiary/victim structures, and extractiveness profiles. The progressive_abrogation reading has the highest extractiveness (complete normative reversal) and most asymmetric extraction (traditional authorities comprehensively delegitimized). The literal_hierarchical reading has near-zero extractiveness for traditional authorities but high extraction from women. The contextual_egalitarian reading occupies an intermediate position with reinterpretive flexibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__progressive_abrogation, institutional, 0.15).
constraint_indexing:directionality_override(quranic_gender_verses__progressive_abrogation, organized, 0.85).
constraint_indexing:directionality_override(quranic_gender_verses__progressive_abrogation, moderate, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
