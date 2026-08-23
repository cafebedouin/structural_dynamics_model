% ============================================================================
% CONSTRAINT STORY: family_law_authority__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__christian_canonical_reading, []).

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
 *   constraint_id: family_law_authority__christian_canonical_reading
 *   human_readable: Christian Canonical Marriage Authority
 *   domain: religious_governance/comparative_law
 *
 * SUMMARY:
 *   This constraint story captures the Christian canonical reading of family
 *   law authority: marriage as sacrament (Catholic) or covenant/ordinance
 *   (Protestant) under ecclesiastical or denominational governance. The
 *   Catholic variant enforces indissolubility with annulment as sole exit;
 *   Protestant variants permit divorce with pastoral oversight. Both claim
 *   church authority over validity against state civil marriage. The
 *   constraint coordinates a transcendent marital ideal while extracting
 *   compliance through identity-fused exit barriers (especially Catholic) and
 *   institutional control over the definition of marriage. The claimed_type
 *   is tangled_rope: genuine coordination (sacramental meaning, community
 *   formation, protection against arbitrary abandonment) coexists with
 *   asymmetric extraction (trapping abuse victims, annulment revenue,
 *   denominational control over LGBTQ+ exclusion, gendered power in tribunal
 *   processes). Active enforcement is required — canonical tribunals,
 *   denominational disciplinary bodies, clergy gatekeeping.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.65).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.75).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Christian Canonical Marriage Authority").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "religious_governance/comparative_law").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, '6af0c86b-7de7-4207-aebe-a7616a76cb7e').
narrative_ontology:cs_kernel_codification('6af0c86b-7de7-4207-aebe-a7616a76cb7e', formalized).
narrative_ontology:cs_authority_grounding('6af0c86b-7de7-4207-aebe-a7616a76cb7e', lineage).
narrative_ontology:cs_interpretation_layer_present('6af0c86b-7de7-4207-aebe-a7616a76cb7e').
narrative_ontology:cs_reading_relation('6af0c86b-7de7-4207-aebe-a7616a76cb7e', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('6af0c86b-7de7-4207-aebe-a7616a76cb7e', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('6af0c86b-7de7-4207-aebe-a7616a76cb7e', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('6af0c86b-7de7-4207-aebe-a7616a76cb7e', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('6af0c86b-7de7-4207-aebe-a7616a76cb7e', foundational, marriage_is_sacrament_under_church_authority).
narrative_ontology:cs_axiom_status(marriage_is_sacrament_under_church_authority, holdable).
narrative_ontology:cs_axiom_grounding('6af0c86b-7de7-4207-aebe-a7616a76cb7e', marriage_is_sacrament_under_church_authority, deontological).
narrative_ontology:cs_axiom('6af0c86b-7de7-4207-aebe-a7616a76cb7e', foundational, church_has_authority_over_marital_validity).
narrative_ontology:cs_axiom_status(church_has_authority_over_marital_validity, holdable).
narrative_ontology:cs_axiom_grounding('6af0c86b-7de7-4207-aebe-a7616a76cb7e', church_has_authority_over_marital_validity, conventional).
narrative_ontology:cs_reference_frame('6af0c86b-7de7-4207-aebe-a7616a76cb7e', divine_sacramental_marriage_under_ecclesial_guardianship).
narrative_ontology:cs_drift_state('6af0c86b-7de7-4207-aebe-a7616a76cb7e', contemporary_secular_hegemony, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6af0c86b-7de7-4207-aebe-a7616a76cb7e', '').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, catholic_magisterium).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, protestant_denominational_authorities).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, canonical_lawyers_tribunals).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, abused_spouses_catholic).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, abused_spouses_protestant).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, catholic_laity_married).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, protestant_laity_married).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, catholic_laity_married).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, protestant_laity_married).
narrative_ontology:constraint_vindicates(family_law_authority__christian_canonical_reading, marriage_as_sacrament).
narrative_ontology:constraint_vindicates(family_law_authority__christian_canonical_reading, ecclesial_authority_over_validity).
narrative_ontology:constraint_vindicates(family_law_authority__christian_canonical_reading, indissolubility_of_sacramental_bond).
narrative_ontology:constraint_vindicates(family_law_authority__christian_canonical_reading, church_as_guardian_of_marriage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and enforces canonical marriage law globally through the Code of Canon Law and papal teaching. Controls the annulment process (declaration of nullity) which is the only exit from a valid sacramental marriage. Collects administrative fees for tribunal processes. Claims authority derives from divine institution and apostolic succession.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, catholic_magisterium, agenda_setter,
    institutional, generational, analytical, universal).

% Govern marriage doctrine and discipline within their denominations (synods, general assemblies, episcopal structures). Permit divorce and remarriage with varying pastoral oversight. Maintain clergy officiation requirements and premarital counseling mandates. Authority derives from scriptural interpretation and denominational polity.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, protestant_denominational_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Bound by canonical indissolubility — no divorce, only annulment (which requires proving defect of consent at inception). Annulment process is lengthy (12-18 months), invasive, and costs $1,000-$3,000. Sacramental theology fuses marital identity with faith identity; exit from marriage feels like exit from church. Gain sacramental grace narrative and community recognition.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, catholic_laity_married, payer,
    moderate, biographical, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, catholic_laity_married, beneficiary).

% Subject to denominational divorce policies — most mainline denominations permit divorce and remarriage with pastoral review; evangelical denominations restrict divorce to adultery/abandonment. Clergy officiation required for 'church wedding.' Premarital counseling mandated. Exit exists but carries social and spiritual costs within faith community. Gain covenant community support and religious framing.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, protestant_laity_married, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, protestant_laity_married, beneficiary).

% Cannot obtain canonical divorce; annulment requires proving defect at wedding (not subsequent abuse). Separation permitted but remarriage forbidden. Diocesan tribunals historically skeptical of abuse as grounds for nullity. Civil divorce obtains legal separation but leaves them canonically married — barred from communion if they remarry civilly. Structural trap: faith identity requires marital permanence; safety requires exit.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, abused_spouses_catholic, payer,
    powerless, immediate, trapped, universal).

% Denominational variance: mainline denominations generally recognize abuse as grounds for divorce; conservative evangelical denominations often do not (citing 'exception clauses' limited to adultery). Pastoral pressure to 'submit' or 'forgive' compounds entrapment. Civil divorce available but may trigger church discipline (excommunication, shunning). Exit options depend entirely on denominational governance structure.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, abused_spouses_protestant, payer,
    powerless, immediate, constrained, national).

% Specialize in canonical matrimonial law; process 35,000+ annulment cases annually worldwide. Earn professional fees and institutional positions. Their expertise is the operational engine of the annulment system — they benefit from the complexity and necessity of the process. Could practice civil family law instead but the canonical niche provides protected market.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, canonical_lawyers_tribunals, beneficiary,
    organized, biographical, mobile, universal).

% Administer civil marriage and divorce law parallel to religious systems. Recognize religious annulments only as evidence for civil purposes (varies by jurisdiction). Increasingly assert state supremacy over child custody, property, and domestic violence protection regardless of religious status. Watch religious accommodation claims (e.g., religious arbitration tribunals) with constitutional scrutiny.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, secular_state_courts, observer,
    institutional, generational, analytical, national).

% Barred from sacramental marriage in Catholic Church and most Protestant denominations (except affirming ones: Episcopal, ELCA, UCC, PCUSA, some Baptist). Their relationships have no standing in the canonical/denominational framework. Would object to indissolubility and heteronormative sacramental theology if seated. Exit means leaving faith tradition or remaining celibate per church teaching.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, lgbtq_catholics_protestants, excluded,
    moderate, biographical, constrained, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a transcendent framework for marital commitment that binds spouses beyond subjective preference, coordinates community recognition and support, and regulates sexual/reproductive ethics through a shared symbolic order. The church as institution holds the boundary of what counts as marriage.
% TRANSFER_FUNCTION: Moves authority over marital validity, exit, and meaning from the couple and the state to the ecclesiastical/denominational authority. The couple surrenders unilateral exit power; the state surrenders exclusive definitional authority. In return, the couple receives sacramental grace narrative, community envelope, and (in Catholic case) indissolubility guarantee.
% ABSENT_VOICES: LGBTQ+ Christians barred from sacramental marriage; abused spouses (especially Catholic) whose safety requires exit the system denies; divorced-and-remarried Catholics barred from communion; women historically excluded from canonical lawmaking and tribunal judging (though now admitted as auditors and judges). These voices are structurally excluded by the authority structure itself.
% DISAPPEARANCE_RATIONALE: If ecclesiastical marriage authority vanished overnight: Catholic annulment system would collapse — millions of second marriages would lack canonical regularization; Protestant denominations would lose doctrinal coherence on sexuality and family; state would become sole marriage regulator; faith communities would lose a primary ritual of belonging. The symbolic and institutional order of Christian family life would reorganize around civil contract or nothing.
% FOUNDING_PROBLEM: Late antique/early medieval: Christian communities needed to distinguish their marital practice from Roman contract marriage (easy divorce, patriarchal control) and from pagan informal unions. The church claimed jurisdiction to protect women from arbitrary repudiation, enshrine indissolubility as witness to Christ-Church union, and regulate consanguinity/incest. Canonical courts emerged to adjudicate validity when civil courts were weak or pagan.
% FOUNDING_PROBLEM_CORROBORATION: Historians (Brundage, Reynolds, Noonan) document the protective origin for women against Roman patria potestas. Feminist theologians (Schüssler Fiorenza, Ruether) attest the founding problem is dead — state law now protects women better than canonical indissolubility; the arrangement persists as gendered control. Catholic magisterium attests the problem is live — secular divorce culture proves the sacramental witness is needed more than ever. No consensus across the beneficiary/victim divide.
narrative_ontology:disappearance_verdict(family_law_authority__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__christian_canonical_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.65: the constraint takes authority over marital exit and meaning from couples and state, transferring it to church structures. Annulment fees, mandatory counseling, clergy gatekeeping, and the indissolubility rule (Catholic) or restrictive divorce grounds (conservative Protestant) all extract compliance. Suppression 0.75: canonical courts actively adjudicate; disciplinary canons penalize 'irregular' marriages; social pressure in faith communities enforces norms. Theater 0.40: sacramental theology is genuinely believed and functionally coordinates, but institutional maintenance (tribunal bureaucracy, denominational polity fights over sexuality) shows performative drift. Accessibility collapse 0.70: Catholic identity-lock is near-total; Protestant constrained exit varies. Resistance 0.55: reform movements (women's ordination, LGBTQ+ affirmation, abuse survivor advocacy) generate internal pressure; secular law exerts external pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the magisterium/denominational seat: this is a rope — genuine coordination of a sacramental reality that protects marriage from commodification. From the abused spouse seat (especially Catholic): this is a snare — the coordination story is cover for trapping victims. From the canonical lawyer seat: this is a scaffold — the annulment process is a temporary pastoral accommodation that became permanent structure. The engine computes these divergences from the structural data; the authored claim (tangled_rope) names the hybrid reality without adjudicating which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Catholic magisterium and Protestant denominational authorities are agenda_setters (d ~0.15 — they administer and benefit from control). Canonical lawyers are beneficiaries (d ~0.2 — professional niche). Catholic laity are payers with identity_locked exit (d ~0.85 — faith identity fused to marital indissolubility). Protestant laity are payers with constrained exit (d ~0.65 — denominational switching possible but costly). Abused spouses Catholic are trapped payers (d ~0.95 — safety vs. salvation bind). Abused spouses Protestant are constrained payers (d ~0.75 — denomination-dependent). Secular state is observer (d ~0.5 — analytical seat). LGBTQ+ Christians are excluded (d not computed — not in the game).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting women from arbitrary repudiation in late antiquity) is substantially solved by modern civil law — state courts now regulate divorce, property, custody, and domestic violence better than canonical courts. Yet the arrangement persists and has layered new extraction: LGBTQ+ exclusion, gender complementarity doctrine, annulment fee structures, clergy sexual abuse enabled by clericalist marital theology. The mandate has atrophied into self-preservation. Mandatrophy is NOT resolved — the church authorities claim the founding problem is live (secular divorce culture), but victims and historians attest it is dead. The constraint persists by fusing its protective origin myth with its current extractive operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'Is the christian_canonical_reading a single coherent constraint, or does the Catholic/Protestant split on indissolubility constitute two structurally distinct constraints under one label?',
    'Decompose into catholic_canonical_reading (indissoluble, annulment-only) and protestant_denominational_reading (dissoluble, pastoral divorce). Compare ε, suppression, and stakeholder structures. If they diverge beyond measurement noise, they are two constraints.',
    'If two constraints, the current story violates ε-invariance (one ε for two structural realities). The kernel family would need an additional edge. The Catholic variant would likely classify as snare for abused_spouses_catholic seat; Protestant variant as tangled_rope or rope depending on denomination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Whether the Catholic/Protestant variance within this reading is a parameter or a structural fracture.').

omega_variable(
    annulment_as_extraction_mechanism,
    'Does the Catholic annulment process function primarily as a pastoral discernment of validity, or as a revenue-generating and control-maintaining extraction mechanism?',
    'Analyze tribunal fee structures vs. actual costs; track outcomes for petitioners with/without canon lawyers; measure correlation between annulment grants and donor status. Compare to civil nullity processes.',
    'If extraction-dominant, the Catholic variant''s ε is higher than authored; the coordination function (discerning validity) is cover. Would strengthen snare classification for the Catholic sub-variant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(annulment_as_extraction_mechanism, empirical, 'Whether the annulment tribunal system is coordination or extraction in practice.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (canonical penalties, civil non-recognition of religious-only marriages) or internalized (theological conscience binding, identity fusion making exit unthinkable)?',
    'Post-exit suppression trajectory: interview Catholics who obtained civil divorce but not annulment — does canonical suppression persist as internalized guilt/exclusion from communion? Track Protestant denomination-switchers — does suppression dissolve or transfer?',
    'If internalized, effective suppression is higher than structural measure — the target carries the constraint after formal exit. This would increase χ for identity_locked seats beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in religious marital constraints.').

omega_variable(
    secular_pressure_as_influence_not_foreclosure,
    'Does secular contractual marriage law foreclose the Christian canonical reading, or merely exert structural pressure (influences relation)?',
    'Track jurisdictions where civil same-sex marriage is mandated — do Catholic/Protestant authorities comply, resist, or schism? Measure defections from denominations that align with secular law vs. those that resist.',
    'If forecloses, the secular_contractual_reading relation should be ''forecloses'' not ''influences''. If influences, the pressure is real but the reading adapts (as Protestant denominations have on divorce).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secular_pressure_as_influence_not_foreclosure, empirical, 'Whether state marriage law logically eliminates or merely pressures religious marital authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flac_cr_tr_t0, family_law_authority__christian_canonical_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(flac_cr_tr_t10, family_law_authority__christian_canonical_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(flac_cr_tr_t20, family_law_authority__christian_canonical_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(flac_cr_tr_t30, family_law_authority__christian_canonical_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(flac_cr_tr_t40, family_law_authority__christian_canonical_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(flac_cr_tr_t50, family_law_authority__christian_canonical_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(flac_cr_be_t0, family_law_authority__christian_canonical_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(flac_cr_be_t10, family_law_authority__christian_canonical_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(flac_cr_be_t20, family_law_authority__christian_canonical_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(flac_cr_be_t30, family_law_authority__christian_canonical_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(flac_cr_be_t40, family_law_authority__christian_canonical_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(flac_cr_be_t50, family_law_authority__christian_canonical_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(flac_cr_su_t0, family_law_authority__christian_canonical_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(flac_cr_su_t10, family_law_authority__christian_canonical_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(flac_cr_su_t20, family_law_authority__christian_canonical_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(flac_cr_su_t30, family_law_authority__christian_canonical_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(flac_cr_su_t40, family_law_authority__christian_canonical_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(flac_cr_su_t50, family_law_authority__christian_canonical_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__christian_canonical_reading, 0.08).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This reading decomposes the family_law_authority kernel along the Christian sacramental/ecclesial axis. The ε values differ substantially: secular_contractual_reading has low ε (state contract, easy exit); muslim_shariat_reading has moderate ε (contract with religious terms, male-initiated divorce); hindu_dharmashastra_reading has variable ε (customary, regionally diverse); this reading has high ε due to identity-locked indissolubility (Catholic) and denominational control (Protestant). The network edges represent doctrinal citation and legal accommodation pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__christian_canonical_reading, institutional, 0.15).
constraint_indexing:directionality_override(family_law_authority__christian_canonical_reading, moderate, 0.65).
constraint_indexing:directionality_override(family_law_authority__christian_canonical_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
