% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__harm_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__harm_threshold_reading, []).

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
 *   constraint_id: speech_protection_kernel__harm_threshold_reading
 *   human_readable: Speech Protection Conditional on Demonstrable Harm (Harm Threshold Reading)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   Speech protection conditional on demonstrable harm to victims is one
 *   reading of the contested speech-protection kernel. This reading
 *   interprets the constitutional guarantee of speech rights as limited by an
 *   obligation to prevent documented harms to identifiable victims. The core
 *   claim: where speech causes demonstrable harm to vulnerable groups, the
 *   state may restrict that speech without violating free speech principles.
 *   This reading has become increasingly influential in constitutional
 *   doctrine globally (hate speech laws, harassment restrictions, content
 *   moderation standards) and represents a genuine interpretive alternative
 *   to absolutist, marketplace, dignity, and democratic-participation
 *   readings of the same kernel. The harm-threshold reading narrower the
 *   scope of protected speech by creating a categorical boundary: if
 *   demonstrable harm exists, protection may be withdrawn. The extractiveness
 *   value (0.58) reflects that this reading operates as a hybrid
 *   coordination-extraction mechanism: it coordinates genuine concern for
 *   victim protection while simultaneously expanding suppression authority
 *   and creating ambiguity in harm definitions that enables institutional
 *   capture. The measurement trajectory shows rising extractiveness and
 *   suppression_requirement over the 30-year interval, indicating that the
 *   harm threshold has expanded to cover broader categories of harm
 *   (dignitary, reputational, emotional) and is being invoked more frequently
 *   by enforcement authorities, shifting the constraint toward Snare
 *   territory. Theater ratio remains moderate (0.48) because harm-threshold
 *   reasoning employs genuine conceptual work and victim testimony rather
 *   than pure performative ritual, distinguishing it from piton-level
 *   theater.
 *
 * KEY AGENTS:
 *   - Harm Threshold Advocates (institutional/arbitrage): Civil rights organizations, victim advocacy groups, international human rights bodies — benefit from authority to define and enforce harm protections. Net beneficiaries through agenda-setting power over harm definitions.
 *   - Targeted Speakers (powerless/trapped): Individual speakers and movements subject to restriction when their speech is determined to cause demonstrable harm. Trapped without recourse; cannot exercise speech right or challenge harm determination through speech.
 *   - Marginal Speakers (moderate/constrained): Speakers in contested zones (identity-based speech, controversial topics, disputed empirical claims) who face high cost to speak and rational incentive to self-censor.
 *   - Enforcement Authority (institutional/constrained): Courts, regulators, platform governance bodies determining harm thresholds and enforcing restrictions. Gain interpretive authority from ambiguity in harm definitions; constrained by duty to govern speech.
 *   - Identified Victim Groups: Those invoked to justify harm restrictions. May benefit from protection (if harms are real) or be instrumentalized (if harm claims are pretextual).
 *   - Speech Margin Zone: The zone of speech most vulnerable to suppression via harm claims — typically political dissent, identity-based speech, scientific controversy.
 *   - Traditional Free Speech Doctrine: The older absolutist framework that persists rhetorically but is functionally displaced by harm-threshold reasoning (piton perspective).
 *   - Analytical Observer: Civilizational perspective risking false summit by naturalizing the harm threshold as an immutable feature of speech governance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, 0.58).
domain_priors:suppression_score(speech_protection_kernel__harm_threshold_reading, 0.62).
domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__harm_threshold_reading, "Speech Protection Conditional on Demonstrable Harm (Harm Threshold Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__harm_threshold_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__harm_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, 'da9a975b-89bf-457d-a345-117030071f95').
narrative_ontology:cs_kernel_codification('da9a975b-89bf-457d-a345-117030071f95', formalized).
narrative_ontology:cs_authority_grounding('da9a975b-89bf-457d-a345-117030071f95', lineage).
narrative_ontology:cs_interpretation_layer_present('da9a975b-89bf-457d-a345-117030071f95').
narrative_ontology:cs_reading_relation('da9a975b-89bf-457d-a345-117030071f95', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('da9a975b-89bf-457d-a345-117030071f95', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('da9a975b-89bf-457d-a345-117030071f95', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('da9a975b-89bf-457d-a345-117030071f95', speech_protection_kernel__democratic_participation_reading, influences).
narrative_ontology:cs_axiom('da9a975b-89bf-457d-a345-117030071f95', foundational, demonstrable_harm_overrides_autonomy).
narrative_ontology:cs_axiom_status(demonstrable_harm_overrides_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('da9a975b-89bf-457d-a345-117030071f95', demonstrable_harm_overrides_autonomy, deontological).
narrative_ontology:cs_axiom('da9a975b-89bf-457d-a345-117030071f95', secondary, harm_threshold_as_interpretable_boundary).
narrative_ontology:cs_axiom_status(harm_threshold_as_interpretable_boundary, holdable).
narrative_ontology:cs_axiom_grounding('da9a975b-89bf-457d-a345-117030071f95', harm_threshold_as_interpretable_boundary, empirically_contingent).
narrative_ontology:cs_reference_frame('da9a975b-89bf-457d-a345-117030071f95', harm_prevention_as_coordinate_goal).
narrative_ontology:cs_drift_state('da9a975b-89bf-457d-a345-117030071f95', contemporary_expansion_phase, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('da9a975b-89bf-457d-a345-117030071f95', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, harm_threshold_advocates).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, identified_victim_groups).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, speakers_under_restriction).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, speech_margin_zone).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED SPEAKER (SNARE) — Subject to restriction without meaningful recourse. When authorities determine speech causes demonstrable harm to identified victim groups, the speaker is trapped: cannot exercise speech right, cannot exit the jurisdiction without material cost, cannot challenge the harm determination through speech itself (doing so may worsen the harm finding). Pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(speech_protection_kernel__harm_threshold_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINAL SPEAKER IN RESTRICTED ZONE (SNARE) — Faces high cost to speak within zones where harm threshold is invoked (controversial topics, speech about identity groups, contested empirical claims). Exit options exist (migrate to different jurisdiction, self-censor, speak about non-controversial topics) but are costly. Self-censorship becomes rational, creating a suppression mechanism without formal prohibition.
constraint_indexing:constraint_classification(speech_protection_kernel__harm_threshold_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HARM THRESHOLD ADVOCATES (ROPE) — Institutional actors (civil rights organizations, harm-reduction advocates, victim representation groups) see this reading as coordination: aligning speech rights with protection of vulnerable groups. They experience the constraint as enabling genuine cooperation on a collective problem (preventing documented harms). Net beneficiaries through arbitrage: access to enforcement authority, agenda-setting power over harm definitions.
constraint_indexing:constraint_classification(speech_protection_kernel__harm_threshold_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ENFORCEMENT AUTHORITY (TANGLED ROPE) — Courts, regulators, and speech-governance institutions face genuinely mixed incentives. They coordinate on a real problem (preventing speech harms) while also extracting authority from ambiguity in harm definitions. Extractiveness comes from the interpretive burden: 'demonstrable harm' is a threshold with no natural boundary. Authority gains leverage from ambiguity, creating institutional capture risk. Constrained exit: duty to enforce speech law.
constraint_indexing:constraint_classification(speech_protection_kernel__harm_threshold_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HARM-LIMITING CONSTITUTIONAL MOVEMENT (SCAFFOLD) — Organized coalitions (international human rights bodies, constitutional commissions, transnational advocacy networks) see this reading as a temporary coordination framework pending deeper institutional reform. The harm threshold is a scaffold: it stabilizes protection for victims in the short term while longer-term frameworks (victim-centered jurisprudence, restorative justice, identity protection mechanisms) develop. Mobile exit: these actors can relocate advocacy focus to alternative frameworks, making the scaffold sunset real.
constraint_indexing:constraint_classification(speech_protection_kernel__harm_threshold_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL FREE SPEECH DOCTRINE (PITON) — The older speech-protection framework (absolutist, marketplace of ideas, speaker-centered) persists as a reference point even as harm-threshold reasoning expands. The traditional doctrine is increasingly performative: invoked rhetorically to defend speech rights while being overridden by harm-threshold restrictions in practice. High theater (appeals to timeless principles) masking low functional role. Piton classification reflects institutional inertia in doctrinal language despite structural shift toward harm-threshold enforcement.
constraint_indexing:constraint_classification(speech_protection_kernel__harm_threshold_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the possibility of speech causing demonstrable harm is presented as an immutable feature of communication: speech acts have effects, effects can harm, preventing harm is a legitimate state function, therefore speech protection must be conditional on harm prevention. This perspective naturalizes the harm threshold as inherent to the speech-governance problem itself. However, the structural data contradicts the mountain classification — the engine will identify this as a false summit, revealing that the 'demonstrable harm' concept is itself a contested, contingent institutional creation requiring interpretation and enforcement, not a natural boundary.
constraint_indexing:constraint_classification(speech_protection_kernel__harm_threshold_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__harm_threshold_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(speech_protection_kernel__harm_threshold_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(speech_protection_kernel__harm_threshold_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, TR),
    TR >= 0.70.

:- end_tests(speech_protection_kernel__harm_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): This reading operates as a genuine coordination mechanism addressing real harms (justifying moderate rather than high extractiveness) while simultaneously creating ambiguity in harm definitions that enables institutional capture and suppression of disfavored speech (elevating extractiveness above pure rope). The measured value reflects both genuine victim protection coordination and authority-expansion extraction. The trajectory shows rising extractiveness over time, indicating that harm categories have expanded and enforcement has intensified, shifting the constraint toward Snare. Suppression (0.62): High. The harm threshold operates as a categorical boundary that suppresses speech when invoked. Suppression is structural: authorities can identify new harm categories, victims can bring harm claims, and once harm is found, speech may be categorically blocked before entering the marketplace of ideas. The rising trajectory indicates expanding suppression categories (dignitary harm, group harm, emotional harm) beyond initial core harms (direct material harm, violence). Theater ratio (0.48): Moderate. The harm-threshold reasoning employs genuine conceptual work and victim testimony; this distinguishes it from purely performative ritual. But the ratio is not low because much of the enforcement involves procedural theater (harm determination proceedings, victim testimony rituals) and because the harm threshold itself is ambiguous and subject to interpretive expansion without clear boundaries. Rising theater over time reflects increasing procedural elaboration around harm determination.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the maximum perspectival divergence across positions. The targeted speaker (powerless/trapped) experiences pure extraction with no coordination benefit — the harm threshold is a mechanism for suppressing their speech. The marginal speaker (moderate/constrained) experiences high-cost suppression without categorical exclusion — rational self-censorship becomes optimal. Harm threshold advocates (institutional/arbitrage) experience coordination — the constraint enables protecting victims from documented harms. The enforcement authority (institutional/constrained) experiences genuine mixed incentives — they coordinate on preventing harm while extracting authority from definitional ambiguity. The constitutional reform movement (organized/mobile) sees a temporary scaffold pending deeper institutional reform. The traditional free speech doctrine (institutional/arbitrage) persists as a rhetorical reference point while being functionally displaced (piton). The analytical observer risks seeing the harm threshold as an immutable natural limit (mountain false summit), when it is actually a contingent institutional creation open to contestation and reform.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values for each perspective are derived from structural position: beneficiary status, victim status, and exit options. Harm-threshold advocates (beneficiaries with arbitrage exit) derive d ≈ 0.15 (low extraction experienced); targeted speakers (victims with trapped exit) derive d ≈ 0.95 (maximum extraction experienced); enforcement authorities (mixed position, constrained exit) derive d ≈ 0.60 (moderate extraction with institutional leverage). The magnitude of perspectival gap (from d=0.15 to d=0.95) indicates that this constraint has high structural asymmetry — different positions experience fundamentally different constraint types (rope vs snare vs tangled rope).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstrable_harm_definition,
    'What constitutes ''demonstrable harm''? Is harm defined by intent, outcome, statistical evidence, victim testimony, causal mechanism, or some combination?',
    'Comparative analysis of judicial decisions defining harm across jurisdictions; tracking of harm definitions that expand or contract over time; evidence of definitional drift toward broader harms (emotional, reputational, dignitary) or narrower (direct, measurable, causal).',
    'If narrowly defined (direct causation required): constraint is closer to Mountain/Rope — harm threshold is objective, boundary is stable. If broadly defined (includes emotional, reputational, dignitary harm): constraint moves toward Snare/Tangled Rope — harm threshold becomes a discretionary filter for suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demonstrable_harm_definition, conceptual, 'Definition of demonstrable harm threshold').

omega_variable(
    harm_threshold_vs_absolutist_boundary,
    'This reading logically forecloses the absolutist reading (speech protection near-categorical) or do both remain live positions held by different institutional actors?',
    'Survey of contemporary constitutional jurisprudence: can courts simultaneously endorse near-categorical speech protection AND condition protection on harm prevention? Or do jurisdictions partition into harm-threshold vs absolutist camps? Historical analysis of constitutional evolution: did harm-threshold reasoning emerge to displace absolutism, or coexist as parallel frameworks?',
    'If forecloses: the two readings are incompatible within any single legal framework; once harm-threshold reasoning is adopted, absolutist protection is logically unavailable. If coexists: both readings remain available as live constitutional options, held by different factions or jurisdictions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_threshold_vs_absolutist_boundary, conceptual, 'Logical relationship between harm-threshold and absolutist readings').

omega_variable(
    victim_group_identification_authority,
    'Who has authority to identify victim groups and certify harm? Victims themselves, advocacy organizations, courts, legislators, or some other authority?',
    'Institutional analysis: which authorities have final say in defining who constitutes a victim group and when harm meets the threshold? Case law tracking: does authority centralize in courts, disperse across agencies, or remain contested?',
    'If victims and their advocates control identification: constraint functions as genuine coordination (victims protect themselves). If courts/legislatures control identification: constraint becomes extraction mechanism (authorities define harm to serve institutional interests, suppress dissent). If contested/distributed: extractiveness is masked by procedural complexity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_group_identification_authority, empirical, 'Authority for identifying victim groups and certifying harm').

omega_variable(
    harm_threshold_expansion_drift,
    'Does the harm threshold expand over time to cover new categories (dignitary harm, group harm, psychological harm, reputational harm)? Or does it remain stable around core physical/material harms?',
    'Longitudinal analysis of judicial decisions defining harm: track which types of harm were initially recognized, which were added over time. Compare across jurisdictions: does harm threshold consistently expand or vary by institutional structure?',
    'If threshold expands: constraint moves from Tangled Rope toward Snare — more speakers captured, more suppression, less genuine coordination. Supports mandate-trophy risk (expansion_axiom_drift). If threshold remains stable: constraint stays as Tangled Rope with genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_threshold_expansion_drift, empirical, 'Historical expansion of harm threshold categories').

omega_variable(
    speech_margin_zone_identification,
    'Which speech zones are most vulnerable to harm-threshold suppression? Political dissent, identity-based speech, scientific controversy, commercial speech, artistic expression?',
    'Analysis of restricted speech across jurisdictions using harm threshold: which types of speech are most frequently blocked? Mapping of the speech margin zone — the zone where harm claims suppress speech most effectively.',
    'If margin zone is narrow and peripheral: constraint is genuine coordination around core harms. If margin zone is broad and includes core political/scientific speech: constraint is an extraction mechanism using harm language to suppress disfavored speech.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(speech_margin_zone_identification, empirical, 'Identity of speech categories most vulnerable to harm-threshold suppression').

omega_variable(
    marketplace_reading_compatibility,
    'Does harm-threshold reasoning coexist with marketplace-of-ideas logic or does it replace it? Can both operate simultaneously — more speech counters harm, but certain speech is categorically blocked before the marketplace operates?',
    'Jurisprudential analysis: do courts cite both harm-threshold and marketplace logic in the same decisions? Or do they partition? Historical evolution: did harm-threshold reasoning emerge as a supplement to marketplace logic or as its replacement?',
    'If coexists: constraint is Tangled Rope — coordination through speech counter-mechanisms plus boundary enforcement. If replaces: constraint is Snare — harm threshold becomes a categorical barrier that prevents speech from entering the marketplace.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marketplace_reading_compatibility, conceptual, 'Relationship between harm-threshold and marketplace-of-ideas readings').

omega_variable(
    false_summit_natural_harm_framing,
    'Is the harm threshold an immutable feature of speech-governance (Mountain), or is it a contingent institutional creation that naturalizes state power over speech content?',
    'Comparative constitutional analysis: do all jurisdictions recognize harm thresholds, or is this particular reading contingent on specific institutional histories? Genealogical analysis: how did harm-threshold reasoning emerge in speech doctrine — from established legal principle or from advocacy by particular groups seeking speech restrictions?',
    'If immutable: harm threshold is a natural boundary, this reading is Mountain, sketch is valid. If contingent: harm threshold is a false summit naturalizing extractive suppression, this reading is Snare/Tangled Rope from all perspectives, sketch misclassifies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_harm_framing, conceptual, 'Whether harm threshold is natural boundary or contingent institutional creation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__harm_threshold_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(spee_tr_t15, speech_protection_kernel__harm_threshold_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(spee_tr_t30, speech_protection_kernel__harm_threshold_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(spee_be_t15, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(spee_be_t30, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(spee_su_t15, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(spee_su_t30, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__harm_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_kernel is instantiated by five structurally distinct constraints, each representing a different reading of the same constitutional kernel. Each reading has its own ε value, its own beneficiary/victim structure, and its own classification type. The harm_threshold_reading (ε=0.58, Tangled Rope) is downstream of and in structural tension with the absolutist_reading (expected ε≤0.20, likely Rope) and democratic_participation_reading. The readings coexist as live institutional positions held by different factions within the judiciary, legislature, and advocacy communities. All five readings affect each other through jurisprudential evolution and institutional policy-setting.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__harm_threshold_reading, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
