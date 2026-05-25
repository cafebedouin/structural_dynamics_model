% ============================================================================
% CONSTRAINT STORY: relational_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_relational_autonomy, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: relational_autonomy
 *   human_readable: Relational Autonomy in End-of-Life Decision Authority
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   Relational autonomy in end-of-life decision-making proposes that dignity
 *   and legitimate authority emerge not from individual choice in isolation
 *   (autonomy-primary model) nor from institutional preservation of life
 *   (sanctity-primary model), but from shared decision-making within a
 *   patient-family-clinician triad, constrained by procedural safeguards and
 *   respect for relational context. This constraint instantiates ONE READING
 *   of a contested kernel (dignified_death). The reading assumes that
 *   relational capacity is distributed across the patient population, that
 *   family involvement is beneficial or at minimum present, and that
 *   procedural safeguards prevent extraction. The constraint models a genuine
 *   coordination mechanism: it distributes decision authority, reduces
 *   unilateral professional gatekeeping, and embeds medical judgment in
 *   relational context. But it also exhibits extractive dynamics,
 *   particularly when applied to patients lacking genuine relational capacity
 *   or whose relational context is corrupted by power asymmetry. The theater
 *   ratio (0.52) reflects that formal consent procedures, capacity
 *   assessments, and ethics committee review are increasingly performative:
 *   they document the appearance of shared decision-making while
 *   institutional preferences and family defaults drive actual decisions. The
 *   extractiveness has risen over time (0.28 to 0.38) as the procedural
 *   apparatus has expanded, converting what began as a genuine relational
 *   practice into an increasingly bureaucratic legitimation ritual.
 *
 * KEY AGENTS:
 *   - Patient-Family Unit: Primary beneficiary (moderate/constrained) — participates in authority distribution; constrained by medical complexity but not trapped
 *   - Clinician-Medical Team: Primary beneficiary (institutional/constrained) — benefits from distributed authority and reduced liability; extraction is modest procedural burden
 *   - Relational Network (abstract): Primary beneficiary — the constraint's existence benefits the idea that relational context matters to dignity
 *   - Vulnerable Patient (cognitive/emotional compromise): Victim (powerless/identity_locked) — appears to participate but cannot exit relational frame that constitutes identity
 *   - Severely Marginalized Patient (undocumented, no family, linguistic isolation): Victim (powerless/trapped) — trapped in a coordination mechanism that presumes relational capacity they lack
 *   - Hospital Administration: Secondary beneficiary (institutional/constrained) — extracts coordination overhead, liability reduction, and procedural legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(relational_autonomy, 0.38).
domain_priors:suppression_score(relational_autonomy, 0.45).
domain_priors:theater_ratio(relational_autonomy, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(relational_autonomy, extractiveness, 0.38).
narrative_ontology:constraint_metric(relational_autonomy, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(relational_autonomy, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(relational_autonomy, rope).
narrative_ontology:human_readable(relational_autonomy, "Relational Autonomy in End-of-Life Decision Authority").
narrative_ontology:topic_domain(relational_autonomy, "bioethics/medical_law/political_philosophy").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(relational_autonomy, relational_network).
narrative_ontology:constraint_beneficiary(relational_autonomy, procedural_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT-FAMILY UNIT (ROPE) — Experiences the constraint as coordination mechanism enabling shared decision-making with procedural safeguards. Constrained by medical complexity and institutional requirements, but benefits from having legitimate voice in decisions affecting their own death. Moderate power reflects genuine agency within distributed decision structure. Exit options are constrained (cannot unilaterally override medical judgment, cannot opt out of dialogue) but this constraint is the mechanism that enables their participation, not one that excludes them.
constraint_indexing:constraint_classification(relational_autonomy, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: CLINICIAN-MEDICAL TEAM (ROPE) — Experiences the constraint as mandatory coordination structure. Constrained by legal and professional obligation to engage in structured dialogue, share information asymmetry, document consent processes. Benefits from the relational framing: removes the burden of unilateral decision authority (pure autonomy model would make clinicians sole gatekeepers of what information to disclose) and distributes responsibility across the triad. The procedural safeguards reduce liability and ethical burden. Experiences moderate extraction in the form of time and documentation requirements, but this is legitimate coordination cost.
constraint_indexing:constraint_classification(relational_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / INSTITUTIONAL VIEW (ROPE) — From a generational and global lens, relational autonomy is a coordination mechanism solving the collective action problem of end-of-life decision authority: it prevents both pure medicalization (clinician sole authority) and pure individualism (patient isolated from relational context). The constraint enables legitimate authority distribution. Extraction is modest and transparent — procedural overhead, documentation burden, time requirements are necessary to the coordination function, not parasitic on it.
constraint_indexing:constraint_classification(relational_autonomy, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: VULNERABLE PATIENT (TANGLED ROPE, IDENTITY-LOCKED) — A patient whose cognitive or emotional capacity is compromised (dementia, depression, trauma history, linguistic isolation) may experience the relational autonomy constraint very differently. Structurally mobile (could theoretically refuse participation or delegate entirely to family), but identity-locked: they cannot imagine themselves as a separate decision-maker apart from family consensus, or their trauma history has fused their identity with deference to authority figures. The constraint becomes extractive — family preferences are laundered as 'relational context' when they actually override the patient's genuine preferences that emerge only in private. The procedural safeguards (informed consent documentation, capacity assessment) are performative theater — the vulnerable patient cannot exit the relational frame that constitutes their identity.
constraint_indexing:constraint_classification(relational_autonomy, tangled_rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 5: SEVERELY MARGINALIZED PATIENT (SNARE) — A patient experiencing severe power asymmetries (undocumented immigrant, no family support, linguistic barrier, severe cognitive impairment, institutionalized) is trapped in the relational autonomy structure. The constraint mandates participation in a 'triad' when no functioning relational unit exists. Family proxies may be absent, untrustworthy, or unavailable. Clinicians bear cultural authority the patient cannot challenge. The relational model extracts from this agent by requiring them to navigate a coordination system they have no capacity to participate in, while the procedural safeguards (informed consent forms in English, family meeting documentation) become pure theater. The agent experiences maximum extraction because the constraint assumes relational capacity that does not exist.
constraint_indexing:constraint_classification(relational_autonomy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 6: HOSPITAL ADMINISTRATION (TANGLED ROPE) — Experiences relational autonomy as a hybrid coordination/extraction mechanism. Genuine coordination function: the relational model reduces liability by distributing decision authority and ensuring consent documentation. But extraction exists: the hospital captures coordination overhead (ethics committees, social work consultation, chaplain time, legal review) and converts it into efficiency gains and liability reduction that benefit the institution more than the patient-family unit. The constraint coordinates but also extracts institutional benefit through the procedural apparatus.
constraint_indexing:constraint_classification(relational_autonomy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(relational_autonomy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(relational_autonomy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(relational_autonomy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(relational_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, rising over interval. The baseline value reflects that relational autonomy solves a genuine coordination problem — pure autonomy models leave patients isolated from relational context; pure sanctity models monopolize authority in medical institutions. The relational model distributes authority. But extractiveness is not zero because the procedural apparatus (ethics committees, consent documentation, capacity assessment, family coordination) converts coordination overhead into institutional benefit. The rise from 0.28 to 0.38 reflects increasing bureaucratization: what began as authentic relational practice has become increasingly formal, and the formalism generates institutional value (liability protection, efficiency in decision logging) that is extracted from the decision-making process itself. Suppression (0.45): Moderate-high. Barriers to patient autonomy exist: medical complexity requires expert interpretation; vulnerable patients cannot navigate the procedural requirements; family involvement can override patient preference while framing it as 'relational'; institutional defaults are embedded in the consent process. But suppression is not total — the constraint provides mechanisms for patient voice, procedural safeguards theoretically prevent pure extraction, and opt-out (refusing to participate in relational frame) is structurally possible if emotionally/identity costly. Theater ratio (0.52): Moderate-high, rising. Formal procedures (informed consent, capacity assessment, ethics consultation) are increasingly divorced from actual decision-making. Many end-of-life decisions are predetermined by family consensus or institutional protocol before the relational apparatus engages. The procedures validate and document rather than determine. The rise from 0.42 to 0.52 reflects that as institutional pressure has mounted to demonstrate relational autonomy compliance, the performative component has increased while authentic relational engagement may have stagnated.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap lies between beneficiary perspectives (patient-family unit, clinicians, relational network) and victim perspectives (vulnerable patients, marginalized patients). Beneficiaries see rope: coordination mechanism distributing authority and respecting relational context. Victims see tangled_rope or snare: the relational framing is extraction apparatus that presumes relational capacity they lack, and procedural safeguards are theater. The gap emerges because the constraint distributes authority TO relational networks that function well, but extracts FROM agents who lack such networks or whose networks are corrupted by power asymmetry. The identity-locked perspective is critical: a patient whose identity is fused with family consensus will NOT experience relational autonomy as extractive, even if their actual preferences (held privately or unarticulated) diverge from family position. They experience it as dignity-affirming because relational participation IS their constituted identity. A severely marginalized patient with no relational unit or a family colored by coercion will experience the same constraint as pure extraction — forced participation in a coordination mechanism that has no genuine relational content.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value derives from their structural position in the decision authority distribution. The patient-family unit benefits from having voice in decisions affecting their death (d ≈ 0.45, beneficiary with constrained exit), but also bears the burden of participating in procedurally complex coordination (d ≈ 0.45). Clinicians benefit from distributed authority reducing unilateral burden (d ≈ 0.35, beneficiary with constrained institutional exit). Vulnerable and marginalized patients are victims (d ≈ 0.85-0.95) because the constraint presumes relational capacity they cannot exercise. The identity-locked patient occupies an unusual position: structurally they could exit the relational frame (declare themselves independent, refuse family involvement), but identity-fusion makes this unthinkable from within their frame. The engine derives d from victim status + identity_locked exit → d ≈ 0.85 (high target position), but this obscures that the patient does not perceive themselves as extracted from — they perceive themselves as authentically participating. This is exactly where identity_locked reveals its analytical value: the perspectival gap between how an identity-locked agent experiences themselves and how the structural analysis measures them.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that relational autonomy is genuinely rope-class (coordinates a collective action problem: who decides end-of-life care when patient, family, and clinician have different information, values, and stakes?), but ONLY for agents with genuine relational capacity. For vulnerable/marginalized patients lacking such capacity, the constraint becomes tangled_rope or snare — it coordinates benefits for the relational network while extracting from agents who cannot participate authentically. The classification is not unified across populations; it is perspectival based on structural position. The false summit risk lies in naturalizing relational autonomy as the universally appropriate model: 'dignity requires relational context for all.' This naturalizes what is actually a reading-specific claim. For some patients (those with functioning relational units), relational autonomy may be genuinely dignity-enhancing. For others (trapped, identity-locked, or with corrupted relational units), it may be extraction apparatus. The constraint story resolves the mandatrophy by refusing unified classification and instead mapping how the same structural arrangement produces different classifications depending on relational capacity, which is observable and measurable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relational_capacity_ambiguity,
    'When does relational autonomy''s coordination function become extractive for agents lacking genuine relational capacity?',
    'Empirical mapping of relational autonomy outcomes disaggregated by patient capacity, family structure, linguistic access, and cultural context. Identification of threshold where procedural safeguards become theater.',
    'If vulnerable/marginalized patients systematically classify as snare or tangled_rope: relational autonomy as implemented is extraction layered with coordination theater. If outcomes are equitable across capacity levels: coordination function is genuine. If intermediate: hybrid model confirmed but with identified victim populations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(relational_capacity_ambiguity, empirical, 'Whether relational autonomy preserves coordination function across varying relational capacities').

omega_variable(
    family_preference_override_mechanism,
    'In practice, how often do ''family preferences'' in relational autonomy decision-making override documented patient preferences?',
    'Retrospective review of end-of-life decision documentation comparing documented patient wishes, family input, and actual clinical decisions. Analysis of cases where family and patient positions diverged.',
    'If family override is rare (<10%): relational model coordinates authentically. If common (>30%): relational framing is extractive mechanism laundering family preference as shared decision. If identity_locked agents show systematically higher override rates: identity-lock dynamic confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_preference_override_mechanism, empirical, 'Frequency and direction of family-patient preference divergence in relational autonomy decisions').

omega_variable(
    procedural_safeguard_efficacy,
    'Do documented consent procedures (informed consent forms, capacity assessments, ethics consultations) actually prevent extraction, or are they theater that legitimizes predetermined decisions?',
    'Prospective study: compare documented safeguard adherence against actual decision-making patterns. Identify cases where procedure was completed but patient''s documented wishes were not honored.',
    'If safeguards prevent extraction: theater_ratio should be lower (<0.35). If safeguards are theater: theater_ratio confirms (0.52 plausible, may be underestimated). If differential by patient capacity: institutional theater is selective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_safeguard_efficacy, empirical, 'Whether procedural safeguards prevent or enable relational autonomy extraction').

omega_variable(
    kernel_sibling_readings,
    'What structural changes if we adopt autonomy_primary (patient decision authority with medical veto) vs. sanctity_primary (life preservation as institutional default) instead of relational_autonomy?',
    'Conceptual analysis: autonomy_primary shifts victims from ''relational network beneficiaries'' to ''vulnerable patients excluded from unilateral authority''; moves ε lower (coordination costs reduced); produces rope or even lower extraction at institutional perspective. Sanctity_primary shifts beneficiaries to medical institutions and produces snare at patient perspective (authority monopoly); produces higher suppression. Relational_autonomy sits as middle ground—appears to distribute authority but procedural apparatus extracts.',
    'If autonomy_primary shows lower ε: relational reading is more extractive than presented, hidden behind coordination theater. If sanctity_primary shows higher suppression: relational reading genuinely improves patient agency compared to default. If all three readings show similar vulnerability patterns disaggregated by capacity: the problem is implementation fidelity across readings, not the reading choice itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_sibling_readings, conceptual, 'Structural comparison: relational_autonomy reading vs. autonomy_primary and sanctity_primary sibling readings').

omega_variable(
    identity_lock_vs_constrained_boundary,
    'For patients with relational identity fusion (family-embedded self-concept), is their trapped classification a genuine identity lock or a constrained exit with high emotional/identity cost?',
    'Longitudinal case study: track patients post-decision. Do they report relief at having participated relationally, or did participation violate their actual preferences? Do they maintain identity fusion post-end-of-life or report fractured identity after institutional pressure to participate in relational model? Does identity-locked classification change if they imagine themselves outside the relational frame?',
    'If genuine identity fusion: identity_locked classification correct; relational autonomy may be adaptive for this group (respects identity constitution). If constrained with emotional cost: should be classified as constrained, not identity_locked; relational autonomy extracts by requiring emotional participation while framing it as dignity-enhancing. Affects whether relational autonomy is coordinate or extractive for this population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_boundary, empirical, 'Distinguishing identity-locked from constrained exits in relational autonomy decisions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(relational_autonomy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(relaut_tr_t0, relational_autonomy, theater_ratio, 0, 0.42).
narrative_ontology:measurement(relaut_tr_t10, relational_autonomy, theater_ratio, 10, 0.48).
narrative_ontology:measurement(relaut_tr_t20, relational_autonomy, theater_ratio, 20, 0.52).

% Extraction over time
narrative_ontology:measurement(relaut_be_t0, relational_autonomy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(relaut_be_t10, relational_autonomy, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(relaut_be_t20, relational_autonomy, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(relational_autonomy, attachment_coordination).
narrative_ontology:affects_constraint(relational_autonomy, autonomy_primary).
narrative_ontology:affects_constraint(relational_autonomy, sanctity_primary).
narrative_ontology:affects_constraint(relational_autonomy, informed_consent_proceduralism).

% DUAL FORMULATION NOTE:
% Relational autonomy is one reading of the dignified_death kernel. Autonomy_primary and sanctity_primary are sibling readings with different ε values and victim/beneficiary structures. All three are end-of-life decision authority constraints but represent distinct positions on the source of legitimate authority. Network edges indicate that each reading affects the others — empirical evidence about relational autonomy's effectiveness influences which reading becomes institutionally dominant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(relational_autonomy, analytical, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
