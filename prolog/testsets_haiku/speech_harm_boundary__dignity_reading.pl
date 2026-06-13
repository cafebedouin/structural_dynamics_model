% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__dignity_reading, []).

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
 *   constraint_id: speech_harm_boundary__dignity_reading
 *   human_readable: Speech Subordination to Dignity (Categorical Exclusion Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested constitutional
 *   kernel: the speech-harm boundary. The dignity reading holds that speech
 *   protection is subordinate to human dignity; speech that denies the
 *   personhood of targeted groups is categorically unprotected, not subject
 *   to case-by-case balancing. This reading conflicts with an absolutist
 *   reading (speech protection operates near-absolutely) and a harm-balancing
 *   reading (speech yields to demonstrated individualized harm via
 *   proportionality tests). The dignity reading emerged most prominently
 *   after WWII in jurisdictions confronting the role of dehumanizing speech
 *   in genocide and political exclusion. It treats dignity as
 *   foundational—not negotiable in speech contests. The constraint's
 *   extractiveness is high (0.68) because it requires speakers of excluded
 *   speech to bear substantial legal and social sanctions; suppression is
 *   correspondingly high (0.72) because the boundary must be actively
 *   enforced against both speakers and challengers to the dignity-first
 *   axiom. Theater ratio is moderate (0.28): the dignity-protection function
 *   is genuine, but enforcement machinery increasingly dedicates resources to
 *   boundary disputes and doctrinal contestation rather than substantive
 *   dignity vindication.
 *
 * KEY AGENTS:
 *   - dignity_protected_groups: vulnerable populations whose equal standing depends on categorical speech exclusions — bear the cost if the reading fails but receive structural affirmation if sustained
 *   - speakers_of_excluded_categories: individuals whose speech falls in excluded categories — bear legal/social sanctions; their exit is speech modification or relocation
 *   - constitutional_dignity_authority: courts, legislatures, agencies enforcing boundaries; define what counts as personhood-denying speech and adjudicate claims
 *   - absolutist_speech_advocates: institutional and individual advocates for near-absolute protection; excluded from boundary-definition; bear the cost of enforcement machinery
 *   - dignity_framework_theorists: scholars whose interpretive work frames rights as dignity-grounded; benefit from institutional adoption but retain mobile exit
 *   - empirical_harm_researchers: document effects of excluded speech on targeted groups; provide evidence justifying categorical exclusions; take observer position
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, 0.68).
domain_priors:suppression_score(speech_harm_boundary__dignity_reading, 0.72).
domain_priors:theater_ratio(speech_harm_boundary__dignity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__dignity_reading, "Speech Subordination to Dignity (Categorical Exclusion Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__dignity_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_harm_boundary__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, '1c49b1d3-9243-499d-b6b9-3c725b6eeaae').
narrative_ontology:cs_kernel_codification('1c49b1d3-9243-499d-b6b9-3c725b6eeaae', fixed_text).
narrative_ontology:cs_authority_grounding('1c49b1d3-9243-499d-b6b9-3c725b6eeaae', lineage).
narrative_ontology:cs_interpretation_layer_present('1c49b1d3-9243-499d-b6b9-3c725b6eeaae').
narrative_ontology:cs_reading_relation('1c49b1d3-9243-499d-b6b9-3c725b6eeaae', speech_harm_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('1c49b1d3-9243-499d-b6b9-3c725b6eeaae', speech_harm_boundary__harm_balancing_reading, influences).
narrative_ontology:cs_axiom('1c49b1d3-9243-499d-b6b9-3c725b6eeaae', foundational, dignity_foundational_over_speaker_autonomy).
narrative_ontology:cs_axiom_status(dignity_foundational_over_speaker_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('1c49b1d3-9243-499d-b6b9-3c725b6eeaae', dignity_foundational_over_speaker_autonomy, deontological).
narrative_ontology:cs_axiom('1c49b1d3-9243-499d-b6b9-3c725b6eeaae', foundational, categorical_speech_exclusion_necessary_for_equal_standing).
narrative_ontology:cs_axiom_status(categorical_speech_exclusion_necessary_for_equal_standing, holdable).
narrative_ontology:cs_axiom_grounding('1c49b1d3-9243-499d-b6b9-3c725b6eeaae', categorical_speech_exclusion_necessary_for_equal_standing, empirically_contingent).
narrative_ontology:cs_reference_frame('1c49b1d3-9243-499d-b6b9-3c725b6eeaae', dignity_as_foundational_constitutional_commitment).
narrative_ontology:cs_drift_state('1c49b1d3-9243-499d-b6b9-3c725b6eeaae', contemporary_digital_speech_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('1c49b1d3-9243-499d-b6b9-3c725b6eeaae', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, dignity_protected_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, constitutional_dignity_authority).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, speakers_of_excluded_categories).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, absolutist_speech_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, dignity_framework_theorists).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, human_dignity_as_foundational_right).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, personhood_constitutive_of_political_standing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of historically marginalized groups whose personhood is affirmed by categorical protection from speech that denies their humanity (religious minorities, ethnic groups, sexual and gender minorities). They receive structural recognition that their dignity is inviolable and cannot be subordinated to another's expressive interest. They cannot exit the protected status; its legitimacy depends on whether the excluded speech would demonstrably destabilize their standing as equal persons.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, dignity_protected_groups, beneficiary,
    moderate, generational, constrained, national).

% Persons whose speech falls within excluded categories (Holocaust denial, incitement to ethnoreligious violence, systematic group defamation claiming personhood-incompatibility). They bear legal and social sanctions. Their exit consists of speech modification (choosing different content) or geographic relocation. Under this reading, their restriction is not a violation requiring justification but a structural condition of membership in a dignity-based polity.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, speakers_of_excluded_categories, payer,
    moderate, biographical, constrained, national).

% Courts, legislatures, and executive agencies that enforce categorical speech exclusions justified by dignity protection. They define the boundaries of excludable speech, adjudicate claims, and maintain the enforcement machinery. They operate under a mandate that dignity cannot be traded for expressive liberty — the boundaries are not negotiable via balancing tests but are constitutive commitments.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, constitutional_dignity_authority, agenda_setter,
    institutional, generational, analytical, national).

% Institutional and individual advocates for near-absolute speech protection who argue that categorical exclusions violate speaker autonomy and chilling effects harm democratic discourse. They are excluded from the decision-making structure that defines which speech is categorically excludable; they contest the authority's framing and would reorganize the constraint if they could. They bear the cost of their speech restrictions even when they do not fall into protected categories, because the enforcement machinery they contest constrains the overall speech ecology.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, absolutist_speech_advocates, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, absolutist_speech_advocates, excluded).

% Constitutional and political theorists whose work frames human rights as grounded in dignity rather than utility or autonomy. The categorical speech exclusion model vindicates their interpretive framework and feeds their research programs. They benefit from institutional adoption of dignity-centered jurisprudence. Their exit is available (they can adopt competing frameworks) but carries professional costs.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, dignity_framework_theorists, beneficiary,
    moderate, generational, mobile, national).

% Researchers studying effects of excluded speech on targeted groups' psychological well-being, political participation, and equal standing. They provide evidence used to justify categorical exclusions. They take no structural position in the constraint but observe and report on the mechanisms by which speech harms are realized.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, empirical_harm_researchers, observer,
    organized, biographical, analytical, national).

% International courts, human rights committees, and treaty bodies that recognize dignity-based speech limitations as consistent with international law (ICCPR Article 20, ECHR jurisprudence). They validate the reading's theoretical legitimacy and provide external corroboration of the dignity framework. They observe rather than enforce domestically but shape the international normative environment.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__dignity_reading, constitutional_dignity_authority).
narrative_ontology:fixing_cost_class(speech_harm_boundary__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared constitutional commitment that human dignity cannot be subordinated to expressive liberty; creates a stable categorical boundary protecting historically vulnerable groups from speech that denies their personhood, enabling their equal political participation without negotiating that standing in speech-balancing contests.
% TRANSFER_FUNCTION: Transfers the right to speak certain categories of speech from individual speakers to a collective constitutional authority; speakers of excluded categories bear legal and social sanctions; dignity-protected groups receive affirmative recognition of inviolable standing; the constitutional authority gains legitimacy by enforcing a non-negotiable commitment.
% ABSENT_VOICES: Absolutist speech-protection advocates are structurally excluded from the authority that defines categorical boundaries—they contest the framing but cannot reshape it without confronting the dignity-first axiom itself. Speakers whose speech falls in excluded categories cannot participate in boundary-definition without accepting the premise that some speech can be categorically unprotected. Empirical harm researchers design evidence to justify existing boundaries rather than contest them.
% DISAPPEARANCE_RATIONALE: If categorical speech exclusions vanished overnight, political participation rights for dignity-vulnerable groups would become negotiable in every speech-balancing context; Holocaust denial, group defamation, and incitement would require individualized harm proof rather than categorical bars; the authority structure enforcing dignity-as-foundational would collapse and be replaced by proportionality balancing. The political standing of protected groups would reorganize from assured to contested.
% FOUNDING_PROBLEM: Societies emerging from genocide, ethno-nationalist violence, and systematic group exclusion face a choice between treating human dignity as a negotiable interest in speech balancing (risking normalization of dehumanizing speech and political exclusion of targeted groups) or treating it as constitutive (categorically protecting it from expressive override). The dignity reading holds that the founding problem is the vulnerability of personhood itself in democracies where speech is politically powerful.
% FOUNDING_PROBLEM_CORROBORATION: Dignity-protected groups and constitutional courts in countries adopting dignity-first frameworks (Germany, Canada, South Africa) attest the founding problem is live and categorical exclusions are necessary. Absolutist speech advocates and some US constitutional scholars attest the problem is overstated and the categorical response creates its own harms (chilling effects, government authority over speech). International human rights bodies corroborate the dignity-centered reading, though not uniformly. Historical scholarship on democratic collapse in societies that treated dehumanizing speech as protected documents the vulnerability that dignity protections aim to address.
narrative_ontology:disappearance_verdict(speech_harm_boundary__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__dignity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__dignity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(speech_harm_boundary__dignity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.68) because the dignity reading imposes categorical restrictions that do not depend on individualized harm proof—speakers of excluded speech bear costs regardless of audience, context, or actual measurable harm in each case. This is a structural feature: the reading trades away speaker autonomy in a category of speech to secure dignity-vulnerable groups' constitutional standing. Suppression is higher (0.72) than extractiveness because the boundary itself must be continuously defended against challenges from absolutist advocates and against boundary-creep pressures (what counts as personhood-denying speech tends to expand in application even if the core category is stable). Theater ratio is moderate (0.28) because the constraint houses both genuine dignity-protection activity and significant institutional performance: courts and authorities spend resources adjudicating which speech falls in excluded categories, defending doctrinal consistency, and managing international human rights pressures—activities that maintain the authority structure as much as the substantive protection. Accessibility collapse is high (0.81) because speakers effectively have no alternative to speech modification or exit once the excluded category is named; alternative forums and private speech carry formal legal consequences. Resistance is sustained (0.74) because absolutist advocates and some institutional actors continuously contest the dignity-first axiom and push for narrower exclusions or balancing tests. The temporal measurements track the constraint's deepening over 81 years: from initial post-WWII emergence (1945, extractiveness 0.35) through steady institutional adoption and doctrinal development (1970–2010) to contemporary digital-era expansion (2026, extractiveness 0.68). Suppression requirement rises faster than extractiveness (0.48 to 0.72 vs 0.35 to 0.68) because the boundary is increasingly contested and requires more active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The dignity_protected_groups and constitutional_dignity_authority seats compute the constraint as legitimate protection of fundamental standing and should classify it closer to rope (genuine coordination solving a founding problem of vulnerability). Speakers_of_excluded_categories and absolutist_speech_advocates compute it as enforced restriction of autonomy without individual-case harm justification and should classify it closer to snare or piton (inertial defense of a once-necessary rule). The engine's per-seat computation from structural data—power, exit options, beneficiary/victim status, directionality—should register this divergence: dignity-protected groups have constrained exit and strong beneficiary status (d near 0.2–0.4); speakers of excluded speech have constrained exit and are in victim status (d near 0.7–0.9); authority sits in instrumental position (d near 0.5). The perspectival gap is not an error; it is the signature of a tangled arrangement that coordinates dignity protection while extracting speaker autonomy.
 *
 * DIRECTIONALITY LOGIC:
 *   Dignity-protected groups benefit from the constraint (low d, negative effective extraction—the constraint subsidizes their standing); speakers of excluded categories are targeted and bear costs (high d, high effective extraction). The constitutional authority enforces both functions simultaneously—it coordinates dignity protection and extracts speaker restrictions from the same mechanism. This is precisely the tangled-rope structure: genuine coordination (dignity-vulnerable groups gain political standing assurance) bundled with asymmetric extraction (speakers lose autonomy). Absolute speech advocates occupy a complex position: they are not formally in victim status (many are not speakers of excluded speech), but their broader speech-protection agenda is constrained by the categorical boundaries. The engine's derivation from beneficiary/victim declarations should produce d values that spread this structure: low d for protected groups, high d for constrained speakers, intermediate d for organized advocates whose ideological commitment to absolutism is identity-locked but who retain some mobile exit options (they can exit the jurisdiction or abandon the absolutist position).
 *
 * MANDATROPHY ANALYSIS:
 *   The dignity reading faces a potential mandatrophy verdict: the founding problem (preventing normalization of dehumanizing speech that enables genocide and political exclusion) is contested as live vs dead. Absolutist advocates argue the problem is overstated in contemporary democracies with robust institutional checks; dignity-protection authorities argue it is continually live as long as dehumanizing rhetoric emerges. The constraint persists because dignity-vulnerable groups and constitutional authorities continue to invest in maintaining the boundaries. There is no sign of atrophy from non-investment (theater_ratio is moderate, not high). However, if empirical research were to establish that categorical exclusions produce no measurable reduction in harmful outcomes for protected groups—or that they increase underground speech and radicalization—the constraint could become a zombie (founding problem dead but constraint persists through institutional inertia). Current evidence does not support mandatrophy: harm researchers document measurable effects of excluded speech on participation and well-being; dignity-vulnerable groups remain politically mobilized in maintaining boundaries; and authority structures treat the commitment as live. The mandatrophy question is whether the constraint's persistence depends primarily on its coordination function (protecting group dignity) or on institutional capture (the authority maintaining its own jurisdiction). This is an empirical and conceptual omega, not a classification signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foundational_axiom_vs_empirical_justification,
    'Is the dignity-foundation a constitutive commitment that stands independent of whether categorical exclusions empirically protect dignity, or is it dependent on empirical validation of protective effects?',
    'If empirical research demonstrates that categorical exclusions produce no measurable dignity gains (or paradoxically increase harms via radicalization), would the dignity authority maintain the categorical boundaries? If yes, the axiom is foundational; if no, it is empirically contingent.',
    'If empirically contingent, the reading collapses into the harm-balancing reading once evidence shifts; if foundational, the constraint persists even against empirical challenge (risk of mandatrophy if founding problem is empirically dead but boundaries persist ideologically).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_axiom_vs_empirical_justification, empirical, 'Whether dignity-foundation operates as a logical axiom or as an empirical claim contingent on protective outcomes.').

omega_variable(
    personhood_denying_speech_boundary_ambiguity,
    'What counts as speech that denies personhood? The boundary between protected critique of groups and unprotected personhood-denial is contested and creeping.',
    'Track appellate decisions and legislative clarifications over time; identify instances where speech initially deemed personhood-denying is later protected (boundary narrowing) or vice versa (boundary expansion). Establish whether the category has stable content or drifts with political majoritarian changes.',
    'If boundary creeps toward suppression of legitimate group critique, extractiveness and suppression increase and the constraint risks reclassification toward snare. If boundary stabilizes through principled doctrine, the tangled-rope classification holds and theater_ratio remains moderate. Boundary instability itself signals constraint vulnerability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(personhood_denying_speech_boundary_ambiguity, empirical, 'Whether the personhood-denial category maintains stable scope or expands/contracts with political pressure.').

omega_variable(
    identity_lock_suppression_mechanism,
    'To what extent is the suppression of absolutist speech advocates internalized (they have adopted the dignity axiom and self-censor) versus structural (external barriers enforce compliance)?',
    'Post-exit behavior: if advocates exit the jurisdiction and continue absolutist speech abroad without persistence of suppression, suppression was structural. If advocates internalize the dignity frame and sustain suppression even in absolute-speech jurisdictions, suppression is partly internalized.',
    'If suppression is substantially internalized, the constraint''s effective suppression is higher than the structural measure (0.72) suggests—the target carries suppression with them. This increases the constraint''s actual extractiveness and may justify reclassification toward snare if internalization is widespread among constrained speakers. If suppression is purely structural, the 0.72 figure is adequate and reflects enforced restriction only.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_suppression_mechanism, empirical, 'Structural versus internalized suppression mechanism among constrained speakers.').

omega_variable(
    alternative_dignity_framings,
    'Are there structurally distinct ways to ground human dignity-protection that would instantiate different constraints with different ε values?',
    'Comparative analysis: dignity-through-equal-participation (political equality reading), dignity-through-welfare-protection (autonomy-minimally-enabled reading), dignity-through-relational-recognition (relational equality reading). Each ground produces different boundaries, different victim-sets, and different extractiveness profiles.',
    'If alternative groundings exist, the unified ''dignity_reading'' is actually a family of constraints that should decompose per ε-invariance principle. Each reading would have its own constraint story, network links, and type. Current story would become one member of a dignity-constraint-family rather than claiming to represent all dignity-based approaches.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_dignity_framings, conceptual, 'Whether the dignity reading''s boundaries and extractiveness are invariant across different dignity-grounding frameworks.').

omega_variable(
    democratic_legitimacy_of_categorical_boundaries,
    'Do speakers of excluded categories have meaningful democratic participation in defining what counts as personhood-denying speech, or is the definition imposed by the authority without their voice?',
    'Institutional audit: do appellate courts, legislatures, or constitutional conventions include representatives of absolutist advocates or speakers of contested-boundary speech in boundary-definition processes? Or are boundaries set unilaterally by majority authority?',
    'If speakers are excluded from boundary definition, the constraint''s legitimacy depends entirely on the axiom (dignity trumps speaker autonomy) rather than consent. This increases the risk of reclassification toward snare if the axiom is questioned. If speakers have voice, the constraint retains more rope-like legitimacy even if asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_of_categorical_boundaries, preference, 'Procedural legitimacy of categorical boundary-definition process.').

omega_variable(
    reading_vs_absolutist_sibling_foreclosure,
    'Does the dignity reading logically foreclose the absolutist reading within a single constitutional framework, or can both coexist as competing doctrines?',
    'Logical analysis: if dignity is foundational and speaker autonomy is foundational, can both be honored in the same framework? The absolutist reading claims yes (autonomy is the primary right; dignity is protected through speaker liberty enabling marginalized voices). The dignity reading claims no (dignity must be primary to have any standing). Can a framework honor both without collapsing into one or the other?',
    'If they genuinely foreclose each other, the reading relation is ''forecloses'' and the two constraints are incompatible—a jurisdiction must choose. If they can coexist (different parties holding different readings), the relation is ''coexists_with'' and both constraint stories remain live. Current omega assumes they coexist, but this is contestable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_absolutist_sibling_foreclosure, conceptual, 'Logical compatibility of dignity-foundation and absolutist-autonomy axioms within one constitutional framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1945, speech_harm_boundary__dignity_reading, theater_ratio, 1945, 0.12).
narrative_ontology:measurement_basis(spee_tr_t1945, projected).
narrative_ontology:measurement(spee_tr_t1970, speech_harm_boundary__dignity_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement_basis(spee_tr_t1970, observed).
narrative_ontology:measurement(spee_tr_t1995, speech_harm_boundary__dignity_reading, theater_ratio, 1995, 0.19).
narrative_ontology:measurement_basis(spee_tr_t1995, observed).
narrative_ontology:measurement(spee_tr_t2010, speech_harm_boundary__dignity_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement_basis(spee_tr_t2010, observed).
narrative_ontology:measurement(spee_tr_t2020, speech_harm_boundary__dignity_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement_basis(spee_tr_t2020, observed).
narrative_ontology:measurement(spee_tr_t2026, speech_harm_boundary__dignity_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(spee_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t1945, speech_harm_boundary__dignity_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement_basis(spee_be_t1945, projected).
narrative_ontology:measurement(spee_be_t1970, speech_harm_boundary__dignity_reading, base_extractiveness, 1970, 0.42).
narrative_ontology:measurement_basis(spee_be_t1970, observed).
narrative_ontology:measurement(spee_be_t1995, speech_harm_boundary__dignity_reading, base_extractiveness, 1995, 0.54).
narrative_ontology:measurement_basis(spee_be_t1995, observed).
narrative_ontology:measurement(spee_be_t2010, speech_harm_boundary__dignity_reading, base_extractiveness, 2010, 0.61).
narrative_ontology:measurement_basis(spee_be_t2010, observed).
narrative_ontology:measurement(spee_be_t2020, speech_harm_boundary__dignity_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement_basis(spee_be_t2020, observed).
narrative_ontology:measurement(spee_be_t2026, speech_harm_boundary__dignity_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(spee_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1945, speech_harm_boundary__dignity_reading, suppression_requirement, 1945, 0.48).
narrative_ontology:measurement_basis(spee_su_t1945, projected).
narrative_ontology:measurement(spee_su_t1970, speech_harm_boundary__dignity_reading, suppression_requirement, 1970, 0.54).
narrative_ontology:measurement_basis(spee_su_t1970, observed).
narrative_ontology:measurement(spee_su_t1995, speech_harm_boundary__dignity_reading, suppression_requirement, 1995, 0.62).
narrative_ontology:measurement_basis(spee_su_t1995, observed).
narrative_ontology:measurement(spee_su_t2010, speech_harm_boundary__dignity_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement_basis(spee_su_t2010, observed).
narrative_ontology:measurement(spee_su_t2020, speech_harm_boundary__dignity_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement_basis(spee_su_t2020, observed).
narrative_ontology:measurement(spee_su_t2026, speech_harm_boundary__dignity_reading, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement_basis(spee_su_t2026, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1945, tn=2026
narrative_ontology:measurement(spee_grid_01, speech_harm_boundary__dignity_reading, accessibility_collapse(class), 1945, 0.71).
narrative_ontology:measurement(spee_grid_02, speech_harm_boundary__dignity_reading, accessibility_collapse(class), 2026, 0.85).
narrative_ontology:measurement(spee_grid_03, speech_harm_boundary__dignity_reading, accessibility_collapse(individual), 1945, 0.68).
narrative_ontology:measurement(spee_grid_04, speech_harm_boundary__dignity_reading, accessibility_collapse(individual), 2026, 0.76).
narrative_ontology:measurement(spee_grid_05, speech_harm_boundary__dignity_reading, accessibility_collapse(organizational), 1945, 0.72).
narrative_ontology:measurement(spee_grid_06, speech_harm_boundary__dignity_reading, accessibility_collapse(organizational), 2026, 0.84).
narrative_ontology:measurement(spee_grid_07, speech_harm_boundary__dignity_reading, accessibility_collapse(structural), 1945, 0.64).
narrative_ontology:measurement(spee_grid_08, speech_harm_boundary__dignity_reading, accessibility_collapse(structural), 2026, 0.82).
narrative_ontology:measurement(spee_grid_09, speech_harm_boundary__dignity_reading, resistance(class), 1945, 0.71).
narrative_ontology:measurement(spee_grid_10, speech_harm_boundary__dignity_reading, resistance(class), 2026, 0.78).
narrative_ontology:measurement(spee_grid_11, speech_harm_boundary__dignity_reading, resistance(individual), 1945, 0.58).
narrative_ontology:measurement(spee_grid_12, speech_harm_boundary__dignity_reading, resistance(individual), 2026, 0.72).
narrative_ontology:measurement(spee_grid_13, speech_harm_boundary__dignity_reading, resistance(organizational), 1945, 0.64).
narrative_ontology:measurement(spee_grid_14, speech_harm_boundary__dignity_reading, resistance(organizational), 2026, 0.76).
narrative_ontology:measurement(spee_grid_15, speech_harm_boundary__dignity_reading, resistance(structural), 1945, 0.62).
narrative_ontology:measurement(spee_grid_16, speech_harm_boundary__dignity_reading, resistance(structural), 2026, 0.74).
narrative_ontology:measurement(spee_grid_17, speech_harm_boundary__dignity_reading, stakes_inflation(class), 1945, 0.55).
narrative_ontology:measurement(spee_grid_18, speech_harm_boundary__dignity_reading, stakes_inflation(class), 2026, 0.74).
narrative_ontology:measurement(spee_grid_19, speech_harm_boundary__dignity_reading, stakes_inflation(individual), 1945, 0.52).
narrative_ontology:measurement(spee_grid_20, speech_harm_boundary__dignity_reading, stakes_inflation(individual), 2026, 0.71).
narrative_ontology:measurement(spee_grid_21, speech_harm_boundary__dignity_reading, stakes_inflation(organizational), 1945, 0.48).
narrative_ontology:measurement(spee_grid_22, speech_harm_boundary__dignity_reading, stakes_inflation(organizational), 2026, 0.66).
narrative_ontology:measurement(spee_grid_23, speech_harm_boundary__dignity_reading, stakes_inflation(structural), 1945, 0.41).
narrative_ontology:measurement(spee_grid_24, speech_harm_boundary__dignity_reading, stakes_inflation(structural), 2026, 0.58).
narrative_ontology:measurement(spee_grid_25, speech_harm_boundary__dignity_reading, suppression(class), 1945, 0.48).
narrative_ontology:measurement(spee_grid_26, speech_harm_boundary__dignity_reading, suppression(class), 2026, 0.71).
narrative_ontology:measurement(spee_grid_27, speech_harm_boundary__dignity_reading, suppression(individual), 1945, 0.42).
narrative_ontology:measurement(spee_grid_28, speech_harm_boundary__dignity_reading, suppression(individual), 2026, 0.68).
narrative_ontology:measurement(spee_grid_29, speech_harm_boundary__dignity_reading, suppression(organizational), 1945, 0.51).
narrative_ontology:measurement(spee_grid_30, speech_harm_boundary__dignity_reading, suppression(organizational), 2026, 0.74).
narrative_ontology:measurement(spee_grid_31, speech_harm_boundary__dignity_reading, suppression(structural), 1945, 0.54).
narrative_ontology:measurement(spee_grid_32, speech_harm_boundary__dignity_reading, suppression(structural), 2026, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__dignity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(speech_harm_boundary__dignity_reading, 0.12).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__harm_balancing_reading).

% DUAL FORMULATION NOTE:
% The speech-harm boundary kernel admits three structurally distinct constraint readings: dignity_reading (this story), absolutist_reading (near-absolute speaker protection; low ε for coordination, low extractiveness), and harm_balancing_reading (proportionality-based; moderate ε). Each reading instantiates different boundaries, different victim-sets, and different extractiveness profiles. The three stories form a constraint family linked by network.affects_constraints. The dignity_reading influences both siblings because it establishes dignity as a competing weight; it forecloses the absolutist_reading within a single framework (if dignity is foundational, absolutism is ruled out); it coexists with the harm_balancing_reading because proportionality tests could empirically validate categorical exclusions. Decomposition is required by ε-invariance: the three readings would produce different εs if measured differently (absolutist reading minimizes ε by interpreting 'speech harm' narrowly; dignity reading maximizes ε by treating categorical exclusion as necessary coordination cost; balancing reading parametrizes ε by harm threshold). Separate constraint stories preserve measurement integrity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__dignity_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
