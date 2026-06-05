% ============================================================================
% CONSTRAINT STORY: informational_self_determination__data_protection_constitutionalized_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_informational_self_determination__data_protection_constitutionalized_reading, []).

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
 *   constraint_id: informational_self_determination__data_protection_constitutionalized_reading
 *   human_readable: Informational Self-Determination: Data Protection Constitutionalized (German/European Reading)
 *   domain: legal/doctrinal/constitutional_data_law
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested kernel:
 *   informational self-determination. The kernel is the principle that
 *   emerged from the 1983 German Constitutional Court judgment
 *   (Volkszählungsurteil), establishing a right to control one's personal
 *   information as an aspect of human dignity and personality. This reading —
 *   the data-protection-constitutionalized reading — traces the path by which
 *   that constitutional right became embedded in regulatory architecture:
 *   from judicial principle through German data protection law through EU
 *   directives and finally into the GDPR. The constraint captures the
 *   structure by which a constitutional doctrine suppresses free-floating
 *   data collection and grounds regulatory authority in the doctrine's
 *   specifics (purpose limitation, minimization, consent). The competing
 *   readings — census_origin (emphasizing the anti-surveillance genesis) and
 *   surveillance_proportionality (emphasizing ongoing constitutional
 *   auditing) — instantiate the same kernel but highlight different aspects
 *   of its evolution and enforcement. This reading emphasizes the
 *   codification path: judgment → doctrine → regulatory machinery.
 *
 * KEY AGENTS:
 *   - Data Subjects: Primary beneficiary (powerless/trapped → moderate/constrained over time) — gain dignity protection, consent rights, purpose limitation as doctrine codifies. Transition from pure extraction to constrained autonomy.
 *   - Collection-First Business Models (Tech Platforms): Primary victim (powerful/mobile, but constrained by EU scope) — extraction imposed via purpose limitation, minimization, and consent requirements. Profitable data-collection models are suppressed.
 *   - Surveillance Administration Apparatus (Government Data Use): Secondary victim (institutional/constrained) — administrative efficiency reduced by minimization and purpose-limitation audits. Data retention for government purposes bounded by constitutional doctrine.
 *   - Data Protection Authorities (DPAs): Beneficiary (organized/constrained) — gain enforcement mandate, legitimacy, and institutional capacity from constitutional doctrine. Coordinate subjects' rights against data collectors.
 *   - Constitutional Court / Karlsruhe: Source beneficiary (institutional/arbitrage) — doctrine extends judicial authority into regulatory domain, provides foundational principle for generational jurisprudence.
 *   - Data Rights Advocacy Coalition: Secondary beneficiary (organized/constrained) — gain leverage for enforcement, litigation rights, public legitimacy through constitutional framing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(informational_self_determination__data_protection_constitutionalized_reading, 0.38).
domain_priors:suppression_score(informational_self_determination__data_protection_constitutionalized_reading, 0.48).
domain_priors:theater_ratio(informational_self_determination__data_protection_constitutionalized_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(informational_self_determination__data_protection_constitutionalized_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(informational_self_determination__data_protection_constitutionalized_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(informational_self_determination__data_protection_constitutionalized_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(informational_self_determination__data_protection_constitutionalized_reading, tangled_rope).
narrative_ontology:human_readable(informational_self_determination__data_protection_constitutionalized_reading, "Informational Self-Determination: Data Protection Constitutionalized (German/European Reading)").
narrative_ontology:topic_domain(informational_self_determination__data_protection_constitutionalized_reading, "legal/doctrinal/constitutional_data_law").

domain_priors:requires_active_enforcement(informational_self_determination__data_protection_constitutionalized_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(informational_self_determination__data_protection_constitutionalized_reading, '7cc78b14-95d9-49fc-a9a0-13c25ac9b5c7').
narrative_ontology:cs_kernel_codification('7cc78b14-95d9-49fc-a9a0-13c25ac9b5c7', fixed_text).
narrative_ontology:cs_authority_grounding('7cc78b14-95d9-49fc-a9a0-13c25ac9b5c7', lineage).
narrative_ontology:cs_interpretation_layer_present('7cc78b14-95d9-49fc-a9a0-13c25ac9b5c7').
narrative_ontology:cs_reading_relation('7cc78b14-95d9-49fc-a9a0-13c25ac9b5c7', informational_self_determination__census_origin_reading, coexists_with).
narrative_ontology:cs_reading_relation('7cc78b14-95d9-49fc-a9a0-13c25ac9b5c7', informational_self_determination__surveillance_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('7cc78b14-95d9-49fc-a9a0-13c25ac9b5c7', foundational, constitutional_principle_embeds_in_regulatory_form).
narrative_ontology:cs_axiom_status(constitutional_principle_embeds_in_regulatory_form, holdable).
narrative_ontology:cs_axiom_grounding('7cc78b14-95d9-49fc-a9a0-13c25ac9b5c7', constitutional_principle_embeds_in_regulatory_form, deontological).
narrative_ontology:cs_axiom('7cc78b14-95d9-49fc-a9a0-13c25ac9b5c7', foundational, dignity_grounds_data_control).
narrative_ontology:cs_axiom_status(dignity_grounds_data_control, holdable).
narrative_ontology:cs_axiom_grounding('7cc78b14-95d9-49fc-a9a0-13c25ac9b5c7', dignity_grounds_data_control, deontological).
narrative_ontology:cs_reference_frame('7cc78b14-95d9-49fc-a9a0-13c25ac9b5c7', constitutional_dignity_principle_as_regulatory_kernel).
narrative_ontology:cs_drift_state('7cc78b14-95d9-49fc-a9a0-13c25ac9b5c7', contemporary_gdpr_enforcement_phase, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7cc78b14-95d9-49fc-a9a0-13c25ac9b5c7', '').
narrative_ontology:cs_kernel_id(informational_self_determination__data_protection_constitutionalized_reading, informational_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(informational_self_determination__data_protection_constitutionalized_reading, data_subjects).
narrative_ontology:constraint_beneficiary(informational_self_determination__data_protection_constitutionalized_reading, individual_dignity_rights_holders).
narrative_ontology:constraint_victim(informational_self_determination__data_protection_constitutionalized_reading, collection_first_business_models).
narrative_ontology:constraint_victim(informational_self_determination__data_protection_constitutionalized_reading, surveillance_administration_apparatus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT UNDER PRE-GDPR REGIME (SNARE) — Before constitutional anchoring, individuals faced free-floating data collection with no legal recourse. No alternatives, no consent framework, no purpose limitation. Trapped in administrative and commercial surveillance with no structural exit. Pure extraction from the subject's perspective — data is collected and used without principle.
constraint_indexing:constraint_classification(informational_self_determination__data_protection_constitutionalized_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONTEMPORARY DATA SUBJECT UNDER GDPR (TANGLED ROPE) — The constitutional doctrine codified into GDPR creates both coordination and constraint. Data subject gains consent rights, purpose limitation, minimization — real beneficiary of the architecture. But also faces friction cost (cookie banners, consent fatigue), data breaches, and administrative burden. Benefits exist (dignity protection, some control) alongside residual extraction (data still collected, still analyzed, friction costs remain). Constrained by practical barriers to exercising rights despite legal framework.
constraint_indexing:constraint_classification(informational_self_determination__data_protection_constitutionalized_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DATA RIGHTS ADVOCACY COALITION (ROPE) — Civil society organizations (privacy advocates, data rights NGOs) experience the constitutional doctrine as coordination mechanism. The doctrine provides leverage for advocacy, enforcement rights, and a stable reference frame for demanding compliance. These actors see the constraint as enabling their capacity to organize and constrain the data economy. Beneficiary + constrained exit = rope classification.
constraint_indexing:constraint_classification(informational_self_determination__data_protection_constitutionalized_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: REGULATORY AUTHORITY / DPA (TANGLED ROPE) — Data protection authorities experience the constitutional doctrine as both enabling and constraining. The doctrine provides their mandate and legitimacy (coordination function), but also requires them to enforce against powerful data-collection actors while managing practical limitations (resource scarcity, jurisdictional gaps). They benefit from institutional authority while constrained by member-state sovereignty and enforcement obstacles. Mixed beneficiary-victim status with active enforcement burden.
constraint_indexing:constraint_classification(informational_self_determination__data_protection_constitutionalized_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: DATA ECONOMY / PLATFORMS (SNARE) — Tech platforms and data-intensive business models experience the constitutional doctrine as suppression of their preferred operational model. Collection-first, consent-optional, use-any-data-for-any-purpose arrangement is blocked by purpose limitation, minimization, and consent requirements. High extraction cost imposed (compliance burden, use-case limitation, technical infrastructure investment). Powerful agents with real exit options (geographic arbitrage to non-GDPR jurisdictions) but still constrained in EU markets. Snare from their perspective: suppression of profitable model with legal/technical barriers to evasion within EU scope.
constraint_indexing:constraint_classification(informational_self_determination__data_protection_constitutionalized_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: PUBLIC ADMINISTRATION (TANGLED ROPE) — Government agencies using data for administrative purposes (welfare, taxation, law enforcement) experience constitutional doctrine as both coordination and constraint. Coordination: the doctrine provides a stable legitimacy frame for data use (purpose limitation bounds their use cases, but also justifies retention). Constraint: minimization and consent requirements slow administrative efficiency and create audit burden. Agencies are neither full beneficiaries nor full victims — they coordinate with the doctrine while experiencing real friction.
constraint_indexing:constraint_classification(informational_self_determination__data_protection_constitutionalized_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CONSTITUTIONAL AUTHORITY (KARLSRUHE) (ROPE) — The source institution (German Constitutional Court and related European authorities) that authored and enforced the doctrine experiences it as pure coordination. The judgment creates legitimacy for their authority, provides a foundational principle (informational self-determination) that anchors future rulings, and extends their institutional reach into regulatory domain. No extraction cost — the institution benefits from the doctrine's existence and enforcement. Beneficiary with arbitrage options = rope.
constraint_indexing:constraint_classification(informational_self_determination__data_protection_constitutionalized_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, informational self-determination might appear as a natural law: human dignity inherently includes control over one's information, emerging inevitably from universal principles of personhood and autonomy. This perspective treats the doctrine as an immutable discovery, not a contingent institutional creation. However, the presence of identifiable beneficiaries (data subjects, constitutional authority) and clear suppression mechanisms (against data economy) signals false summit — the natural law framing naturalizes a contingent political victory.
constraint_indexing:constraint_classification(informational_self_determination__data_protection_constitutionalized_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(informational_self_determination__data_protection_constitutionalized_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(informational_self_determination__data_protection_constitutionalized_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(informational_self_determination__data_protection_constitutionalized_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(informational_self_determination__data_protection_constitutionalized_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38, moderate): The doctrine does suppress data collection, but does not eliminate it. Purpose limitation, minimization, and consent requirements all impose friction costs on collection. However, consent mechanisms themselves create extraction vectors (dark patterns, consent fatigue, simulated autonomy). Residual extraction remains: data is still collected at scale, still analyzed, and the burden of exercising rights falls on subjects. The value (0.38) reflects that real suppression of collection-first models exists, but extraction persists as platforms develop consent-frame workarounds and as government administration continues data retention practices justified by purpose-limitation doctrine. Suppression (0.48, moderate-high): The constitutional doctrine creates systematic suppression of free-floating data use through three mechanisms: (1) purpose limitation (data use must fit the declared purpose), (2) minimization (collection scope bounded by necessity), (3) consent (active permission required). These are material barriers to many profitable data uses. However, suppression is not total: vast scope remains for lawful data processing, vague purposes are upheld, and DPA enforcement is resource-constrained. Theater ratio (0.55, moderate): Contemporary GDPR compliance includes substantial performative elements (cookie banners with dark-pattern design, privacy policies written for legal defense rather than clarity, compliance theater that lacks enforcement teeth). But functional elements exist: DPA investigations, significant fines, some forced data-practice changes by platforms. The ratio reflects that the regulatory architecture is neither purely performative nor entirely functional — cookie banners are theater, DPA enforcement is functional, purpose-limitation doctrine is codified and judicially reviewable (functional at the doctrinal level, theater at the implementation level). Tangled rope classification: The constraint provides genuine coordination function (doctrine stabilizes what data use is legitimate), requires active enforcement (DPA audits, compliance machinery), benefits data subjects and advocacy coalitions (beneficiaries), and imposes extraction costs on data collectors and some government actors (victims). The mixture of beneficiary, victim, enforcement, and coordination function maps to tangled rope gates.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The data subject sees the transformation from snare (trapped, no alternatives, pure extraction) to tangled rope (constrained, real rights, residual friction). The platform sees the inverse: from rope (coordination, profitable innovation) to snare (suppression of preferred model, legal barriers to evasion within EU scope). The constitutional authority sees rope throughout (coordination function expands with regulatory codification). The DPA sees tangled rope (coordination mandate + enforcement burden). The public administration sees constraint (efficiency loss + legitimacy gain). The analytical observer risks naturalizing a contingent political victory as a universal principle (mountain perspective). This perspectival structure is diagnostic of the constraint's hybrid nature: it redistributes control without eliminating data use entirely. No single perspective captures the full structure — the presheaf over observation sites is necessary.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation proceeds from beneficiary/victim declarations and exit options. Data subjects shift from trapped (pre-1983) to constrained (contemporary): they gain consent rights and legal recourse but face practical barriers (consent fatigue, friction costs). The constitutional authority (Karlsruhe) experiences arbitrage options: they can apply the doctrine expansively (more authority), contract it (less authority), or maintain it. Data platforms experience mobile options: they can comply within EU (constrained extraction cost), arbitrage to non-GDPR jurisdictions (partial exit), or contest in courts (ongoing suppression cost). DPAs experience constrained exit: they enforce the doctrine as given (cannot rewrite constitutional principle), but have discretion in enforcement intensity (constrained within bounds set by doctrine). The perspectival gap widens when examining how the same doctrine appears: to data subjects as liberation (snare → tangled rope), to platforms as suppression (rope → snare from their view), to constitutional authority as success (rope). The dog-leg in classification (powerless subject sees snare pre-1983, tangled rope post-GDPR; powerful platform sees rope pre-1983, snare post-GDPR) reveals the doctrine's redistributive function — it shifts the balance of control.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the classification is stable across time horizons but shifts across power atoms and exit options. From the data subject's perspective, the constraint transitions from snare (pre-1983) through tangled rope (contemporary) to potentially rope (if consent mechanisms become truly functional). From the platform's perspective, it moves from rope (profitable coordination) to snare (suppressed model). The constitutional authority experiences rope throughout. The mandatrophy question — 'is this coordination or extraction?' — has no single answer because the doctrine accomplishes both functions simultaneously: it coordinates legitimate data use (by anchoring it to purpose limitation and minimization) while extracting costs from collection-first models. The doctrine is mandatrophic by design: it suppresses one form of coordination (collection-first, use-any-data) while enabling another (purpose-limited, consent-gated). The resolution is not to find a single type but to recognize that the constraint's function varies by perspective. The analytical observer's mountain classification is revealed as false summit: the doctrine is not an immutable law but a contingent political victory that benefits data subjects and the constitutional authority at the cost of data collectors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_kernel_vs_regulatory_derivative,
    'Is the constitutional right (informational self-determination) the kernel, with GDPR as derivative codification? Or is GDPR the kernel, with constitutional framing as post-hoc legitimation?',
    'Historical trace: judicial reasoning in Karlsruhe (1983) vs. legislative history of GDPR. Did courts derive doctrine from constitutional principles, or did legislators adopt regulatory framing and courts subsequently constitutionalized it? Chronological and causal analysis.',
    'If constitutional-first: doctrine is immutable (higher accessibility_collapse, lower resistance). If regulatory-first: doctrine is contingent institutional arrangement (susceptible to political reversal if constitutionalization is rhetorical rather than binding).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_kernel_vs_regulatory_derivative, empirical, 'Whether constitutional principle or regulatory framework is the true kernel').

omega_variable(
    consent_as_extraction_mechanism,
    'Do consent mechanisms (cookie banners, consent forms, privacy policies) represent genuine exercise of informational self-determination or a new extraction mechanism that simulates consent while enabling data collection?',
    'Empirical analysis of consent patterns: proportion of active vs passive consent, typical consent rates, correlation between consent requests and subsequent data use. Qualitative analysis of consent interface design (dark patterns, friction asymmetry).',
    'If genuine autonomy: tangled_rope classification holds (coordination function + residual extraction). If simulated: extractiveness rises (0.38 → 0.55+), suppression of autonomous refusal rises, snare reclassification likely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_as_extraction_mechanism, empirical, 'Whether consent frameworks enable autonomy or simulate it').

omega_variable(
    purpose_limitation_enforceability_gap,
    'Is purpose limitation enforced with sufficient specificity to bind data use, or do vague ''business purposes'' and ''compatible use'' doctrines create loopholes that collapse the doctrine''s suppression function?',
    'DPA enforcement data: rate of violations found, sanctions imposed, types of purpose-limitation violations challenged. Correlation between GDPR enforcement and actual data-use curtailment in tech platforms.',
    'If enforced: suppression remains at 0.48, doctrine is functional constraint. If unenforced: suppression drops (enforcement machinery atrophies), extractiveness rises, constraint degrades to piton (theater without function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(purpose_limitation_enforceability_gap, empirical, 'Whether purpose limitation is enforced in practice').

omega_variable(
    reading_specificity_to_constitutionalized_path,
    'This reading instantiates the constitutionalized path: right → doctrine → regulatory architecture. Would census_origin_reading and surveillance_proportionality_reading share this causal sequence, or do they trace different constitutional trajectories?',
    'Textual analysis of the three readings'' specified kernels and how each traces the evolution from judgment to contemporary practice. Mapping the causal and legitimacy chains each reading commits to.',
    'If all three share the constitutionalized path: the reading distinction is about scope or emphasis, not structure. If distinct paths: reading_relations may require revision from coexists_with toward influences or forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_specificity_to_constitutionalized_path, conceptual, 'Whether sibling readings share the constitutionalized causal path').

omega_variable(
    false_summit_digital_rights_naturalization,
    'Does the mountain perspective (civilizational/analytical) naturalize what is actually a contingent political and doctrinal victory? Are digital rights treated as ''inherent to human dignity'' when they are actually constructed through specific judicial reasoning and regulatory choices?',
    'Comparative legal analysis: jurisdictions that rejected or delayed the constitutional framing (US vs EU divergence on privacy doctrine). Counterfactual: what regulatory architecture would exist in absence of Karlsruhe judgment? Historical records of litigation strategy and judicial persuasion.',
    'If naturalization confirmed: mountain classification is false summit. Engine''s false_summit_mountain signature fires, reclassifying to tangled_rope (doctrine provides coordination function AND suppresses collection-first models).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_digital_rights_naturalization, conceptual, 'Whether naturalization of contingent political victory occurs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(informational_self_determination__data_protection_constitutionalized_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infoself_dpconst_tr_t0, informational_self_determination__data_protection_constitutionalized_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(infoself_dpconst_tr_t5, informational_self_determination__data_protection_constitutionalized_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(infoself_dpconst_tr_t10, informational_self_determination__data_protection_constitutionalized_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(infoself_dpconst_tr_t15, informational_self_determination__data_protection_constitutionalized_reading, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(infoself_dpconst_be_t0, informational_self_determination__data_protection_constitutionalized_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(infoself_dpconst_be_t5, informational_self_determination__data_protection_constitutionalized_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(infoself_dpconst_be_t10, informational_self_determination__data_protection_constitutionalized_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(infoself_dpconst_be_t15, informational_self_determination__data_protection_constitutionalized_reading, base_extractiveness, 15, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(infoself_dpconst_su_t0, informational_self_determination__data_protection_constitutionalized_reading, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(infoself_dpconst_su_t5, informational_self_determination__data_protection_constitutionalized_reading, suppression_requirement, 5, 0.25).
narrative_ontology:measurement(infoself_dpconst_su_t10, informational_self_determination__data_protection_constitutionalized_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(infoself_dpconst_su_t15, informational_self_determination__data_protection_constitutionalized_reading, suppression_requirement, 15, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(informational_self_determination__data_protection_constitutionalized_reading, information_standard).
narrative_ontology:affects_constraint(informational_self_determination__data_protection_constitutionalized_reading, informational_self_determination__census_origin_reading).
narrative_ontology:affects_constraint(informational_self_determination__data_protection_constitutionalized_reading, informational_self_determination__surveillance_proportionality_reading).
narrative_ontology:affects_constraint(informational_self_determination__data_protection_constitutionalized_reading, gdpr_consent_mechanisms_functionality).
narrative_ontology:affects_constraint(informational_self_determination__data_protection_constitutionalized_reading, data_protection_authority_enforcement_capacity).

% DUAL FORMULATION NOTE:
% This story is one reading of the informational_self_determination kernel. The sibling readings (census_origin, surveillance_proportionality) are separate constraint stories with their own extractiveness values and perspectives. This reading emphasizes the codification path (judgment → doctrine → regulatory architecture). The siblings trace different evolutionary emphases. All three are linked through network.affects_constraints to mark their interdependence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
