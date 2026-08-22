% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__temporal_accommodation_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eternal_marriage_covenant__temporal_accommodation_reading
 *   human_readable: Manifesto Suspension of Polygamous Marriage Practice (Temporal Accommodation Reading)
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   The Church of Jesus Christ of Latter-day Saints (LDS) presents the 1890
 *   Manifesto as a principled suspension of plural marriage practice in
 *   response to federal legal pressure (statehood conditions, property
 *   seizure, prosecution). In this temporal_accommodation_reading, the
 *   institutional leadership issued a manifesto suspending the practice while
 *   maintaining the doctrine: the principle of eternal marriage (D&C 132) is
 *   eternally true and divinely established, but obedience to the law of the
 *   land takes temporal precedence. The constraint models the resulting
 *   arrangement: practitioners who enter plural marriages face institutional
 *   discipline and exclusion; the doctrine remains embedded in sacred texts
 *   and ceremonies but is officially dormant. The reading claims this is a
 *   temporary accommodation awaiting future restoration when political
 *   constraints lift — the institutional authority structure preserved both
 *   the legal obedience (pacifying federal authority) and the doctrinal claim
 *   (maintaining the eternal principle intact). The competing readings frame
 *   this differently: immutable_commandment_reading asserts polygamy is
 *   eternally required and the Manifesto violated divine law;
 *   prophetic_override_reading asserts the living prophet's authority to
 *   supersede prior revelation makes the Manifesto a valid cancellation of
 *   the prior commandment. This story models the
 *   temporal_accommodation_reading's own structural claim: a suspension
 *   pending restoration, not a repudiation.
 *
 * KEY AGENTS:
 *   - institutional_leadership (Church president and apostolic council): agenda-setter, maintains both doctrinal authority and temporal compliance
 *   - practitioners_of_plural_marriage (members continuing or aspiring to practice): payer/victim, disciplined for obedience to doctrine over institutional suspension
 *   - polygamous_families (existing plural households): victim, disrupted by institutional discipline and social pressure
 *   - federal_government (statehood authority): external agenda-setter enforcing suppression via legal threat
 *   - rank_and_file_membership: beneficiary of institutional legitimacy and statehood-enabled social integration; cost-bearer of doctrinal ambiguity
 *   - apostate_communities (Fundamentalist LDS): victim/excluded, claim true adherence to unsuspended doctrine while institutional reading suspends it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.42).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.68).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, scaffold).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "Manifesto Suspension of Polygamous Marriage Practice (Temporal Accommodation Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).
narrative_ontology:has_sunset_clause(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, '183d31cb-5074-411d-9bf6-97a4738a70d0').
narrative_ontology:cs_kernel_codification('183d31cb-5074-411d-9bf6-97a4738a70d0', fixed_text).
narrative_ontology:cs_authority_grounding('183d31cb-5074-411d-9bf6-97a4738a70d0', lineage).
narrative_ontology:cs_interpretation_layer_present('183d31cb-5074-411d-9bf6-97a4738a70d0').
narrative_ontology:cs_reading_relation('183d31cb-5074-411d-9bf6-97a4738a70d0', eternal_marriage_covenant__immutable_commandment_reading, coexists_with).
narrative_ontology:cs_reading_relation('183d31cb-5074-411d-9bf6-97a4738a70d0', eternal_marriage_covenant__prophetic_override_reading, influences).
narrative_ontology:cs_axiom('183d31cb-5074-411d-9bf6-97a4738a70d0', foundational, temporal_law_supremacy_during_persecution).
narrative_ontology:cs_axiom_status(temporal_law_supremacy_during_persecution, holdable).
narrative_ontology:cs_axiom_grounding('183d31cb-5074-411d-9bf6-97a4738a70d0', temporal_law_supremacy_during_persecution, deontological).
narrative_ontology:cs_axiom('183d31cb-5074-411d-9bf6-97a4738a70d0', foundational, doctrine_survives_suspended_practice).
narrative_ontology:cs_axiom_status(doctrine_survives_suspended_practice, holdable).
narrative_ontology:cs_axiom_grounding('183d31cb-5074-411d-9bf6-97a4738a70d0', doctrine_survives_suspended_practice, theological).
narrative_ontology:cs_axiom('183d31cb-5074-411d-9bf6-97a4738a70d0', secondary, restoration_pending_future_legitimacy).
narrative_ontology:cs_axiom_status(restoration_pending_future_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('183d31cb-5074-411d-9bf6-97a4738a70d0', restoration_pending_future_legitimacy, conventional).
narrative_ontology:cs_reference_frame('183d31cb-5074-411d-9bf6-97a4738a70d0', eternal_plural_marriage_principle).
narrative_ontology:cs_drift_state('183d31cb-5074-411d-9bf6-97a4738a70d0', post_manifesto_federal_accommodation, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('183d31cb-5074-411d-9bf6-97a4738a70d0', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, institutional_leadership).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, practitioners_of_plural_marriage).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, polygamous_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, rank_and_file_membership).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, rank_and_file_membership).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, apostate_communities).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, obedience_to_temporal_law_principle).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, doctrinal_dormancy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Church president and apostolic council issues the Manifesto, frames it as principled accommodation of temporal law over doctrinal practice, maintains the eternal doctrine in texts and temple ceremonies, and enforces suppression of plural marriage through institutional discipline (excommunication, temple denial, social exclusion). They set the interpretation: suspension is temporary; restoration is pending. They collect the benefit: statehood achieved, institutional legitimacy secured, federal threat neutralized. They incur the cost of maintaining doctrinal ambiguity and managing practitioner dissent.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Members who entered into or aspired to plural marriage following the pre-Manifesto doctrine now face institutional discipline for practicing what they were taught as eternal and required. Their identity is fused with both the faith (community, salvation theology, self-concept) and the doctrine (plural marriage as exaltation requirement). Exit from the faith carries social cost (family rupture, community loss); exit from the doctrine while remaining in the faith is institutionally unavailable (the doctrine is affirmed in temple ordinances). They experience institutional suppression as coercive state power co-opted by leadership, not as principled accommodation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, practitioners_of_plural_marriage, payer,
    powerless, biographical, identity_locked, national).

% Existing plural households were disrupted by the Manifesto. Children born into plural arrangements faced institutional exclusion; wives faced abandonment or institutionally-imposed status ambiguity; husbands faced impossible choices (divorce per the Manifesto's directive, or face excommunication). Families were geographically and socially trapped in communities where the practice had been normative. They bear the extraction through family disruption, legal vulnerability, and social stigma.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, polygamous_families, payer,
    powerless, generational, trapped, regional).

% Church members benefit from statehood legitimacy (the institution gained full legal standing and social acceptance), institutional growth and integration into mainstream American society, and access to religious ordinances and community. They internalize the accommodation as a higher principle: obedience to temporal law as a divine value. They incur the cost of doctrinal ambiguity (the principle is affirmed but practice is prohibited) and suppression (the doctrine is sacralized in temple ceremonies but practitioners are disciplined if they live it). Exit from the faith is possible but carries social cost (family rupture, community loss).
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, rank_and_file_membership, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, rank_and_file_membership, payer).

% Fundamentalist LDS communities (primarily in Utah, Arizona, Colorado City enclaves) claim adherence to the unsuspended doctrine and continue practicing plural marriage in defiance of institutional suppression. They are structurally excluded from the institution's sacramental apparatus, disciplined through excommunication and social pressure, and targeted by law enforcement (the institution cooperates with authorities). They maintain the immutable_commandment_reading: the doctrine is eternally binding and the Manifesto violated divine law. Their exit is regional/cultural (leaving means relocation and identity rupture) but they have no institutional voice.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, apostate_communities, excluded,
    moderate, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, apostate_communities, payer).

% Federal authority (Congress, DOJ, territorial governors) enforced suppression of plural marriage through Morrill Act, Edmunds Act, and statehood conditions. The institution negotiated the Manifesto as the exit from federal coercion (military presence, property seizure, prosecution). Federal authority remains external to the institutional reading but constrains what readings are institutionally viable. The federal government treats the Manifesto as permanent renunciation and prosecutes any plural marriage as violating it, regardless of the institutional distinction between doctrine and practice.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, federal_government, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__temporal_accommodation_reading, institutional_leadership).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__temporal_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Political accommodation: the institution required statehood legitimacy to operate freely and grow; the federal government required cessation of plural marriage as a condition of statehood. The Manifesto achieved both by suspending the practice while preserving the doctrine, allowing the institution to claim doctrinal intactness and the federal government to claim legal compliance.
% TRANSFER_FUNCTION: The constraint moves authority from individual members (who previously claimed direct revelation to practice plural marriage) to institutional leadership (who now mediate the interpretation of eternal doctrine through temporal accommodation). Practitioners who claimed doctrinal right to plural marriage transfer their trust to institutional authority in exchange for the promise of future restoration. The institution transfers its external legitimacy threat (federal coercion) into an internal doctrinal principle (obedience to law of the land supersedes individual revelation).
% ABSENT_VOICES: Apostate/Fundamentalist communities (excluded from the institutional framework) would object that the doctrine is eternally binding and the Manifesto violated divine law. Women whose plural marriages were dissolved or placed in ambiguous status would object to the gendered cost structure (men could exit plural arrangements more easily than women could). Federal authorities, while external, would reject the institutional claim that the suspension is temporary (federal law treats plural marriage as permanently prohibited). Lower-status practitioners in remote communities where plural marriage had become cultural practice would object to the urban-leadership-made accommodation.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared (the Manifesto was rescinded and plural marriage practice was re-legalized), the federal-state accommodation would collapse; the institution would face legal prosecution; statehood status and mainstream legitimacy would be at risk. Simultaneously, practitioners in apostate communities would experience institutional validation and re-integration. Polygamous families would see legal recognition of existing arrangements. Rank-and-file membership would face a choice between renewed doctrinal practice and federal law. The world rearranges because the constraint props up an institutional accommodation between federal authority and religious authority; its disappearance would reactivate the conflict the Manifesto suspended.
% FOUNDING_PROBLEM: Federal government prohibited plural marriage as a condition of Utah territorial incorporation and statehood. The institution practiced plural marriage as doctrine (D&C 132) and as lived practice across the membership. The conflict threatened institutional survival (property seizure, prosecution, exclusion from statehood). The founding problem was: how can the institution maintain its doctrinal claim while achieving political legitimacy?
% FOUNDING_PROBLEM_CORROBORATION: Institutional leadership affirms the founding problem is solved: statehood achieved, federal threat neutralized, doctrine preserved. Federal authorities and historians affirm the founding problem is solved: plural marriage is legally prohibited and the Manifesto's permanent renunciation achieved that. Practitioners of plural marriage contest that the founding problem is solved: the doctrine remains eternally true and practitioners are prohibited from living it, so the problem is inverted (the doctrine is unsuspended but practice is suppressed). Apostate communities affirm the founding problem persists: the institutional accommodation violates the doctrine and leaves practitioners in an untenable position. External religious scholars note that the founding problem's resolution is frame-dependent: each reading defines what 'solving' the problem means differently.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__temporal_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__temporal_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).
:- end_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the constraint solves a genuine coordination problem (federal statehood required institutional legitimacy; plural marriage was incompatible with statehood) but does so by imposing discipline on practitioners who were previously compliant with doctrinal teaching. The burden falls asymmetrically on practitioners. Suppression is high (0.68) because enforcement depends actively on institutional discipline mechanisms (excommunication, social exclusion, denial of temple access — ordinances central to the faith's salvation theology). The suppression is internalized as well as structural: members who accept the institutional reading internalize the doctrine-vs-law hierarchy; members who reject it face community isolation. Theater_ratio is high (0.58) because significant institutional activity after the Manifesto appears performative: the doctrine remains sacralized in texts and rituals, annual affirmations of the principle occur in temple ceremonies, leadership statements periodically revisit the eternal status of the doctrine, yet 130+ years have passed with no institutional movement toward restoration. The performative activity defends the claim that restoration is pending without confronting the structural question: has the restoration pathway atrophied? The measurement series captures the immediate post-Manifesto intensification (time 0-4 represents 1880-1900, the crisis and settlement period), then stabilization as statehood was achieved (time 4-16 represents 1900-2000, the long institutional accommodation). Extractiveness and theater flatten after statehood, indicating the constraint settled into a stable equilibrium rather than escalating.
 *
 * PERSPECTIVAL GAP:
 *   Institutional_leadership reads this as rope: genuine coordination achieved (statehood enabled, federal threat neutralized) with minimal coercive overhead (the Manifesto was necessary, not gratuitous). Practitioners_of_plural_marriage read this as snare: the doctrine was taught as eternal and required; the suspension is coercive state power co-opted by institutional leadership to suppress a practice they present as their foundational sacrament. Rank_and_file_membership experiences it as tangled_rope: the coordination function is real (statehood, legitimacy, social integration), but the extraction persists through doctrinal suppression and identity-fusion (the faithful internalize the accommodation as a higher principle; obedience to law supersedes individual revelation). Apostate_communities read this as snare with institutional cover: the doctrine remains sacralized to keep practitioners bound to the institution, but the suspension is enforced such that practitioners cannot freely choose to live the doctrine; the institution collects their loyalty while denying their access to the practice. The engine computes per-seat type from the structural data; the authored claim (scaffold) represents the institutional_leadership's own framing — a transient accommodation that preserves the principle and awaits restoration. The divergence between claim and computed types is exactly what the framework measures.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional_leadership holds d ≈ 0.15 (near beneficiary end): they collect the benefit of statehood legitimacy, maintain doctrinal authority, and incur costs only to the extent of managing the contradiction. They have arbitrage-grade exit (could theoretically abandon the doctrine or refuse statehood; they chose the accommodation frame). Practitioners_of_plural_marriage hold d ≈ 0.85 (near full target end): they bear the extraction directly through institutional discipline, face identity-locked exit (the faith is constitutive of their self-concept and community membership), and have no alternative interpretation available within the institutional framework. Federal_government holds d analytically: they are the external enforcer but not party to the internal faith structure. Rank_and_file_membership sits near symmetric (d ≈ 0.50): they benefit from statehood legitimacy and social integration (major benefit), incur the cost of doctrinal ambiguity and suppression of an openly-taught principle (moderate cost), and have constrained exit (leaving the faith carries social cost but is possible). Apostate_communities hold d ≈ 0.90 (near full target): they are structurally excluded from the institutional framework, disciplined for maintaining the unsuspended doctrine, and have no voice in the institutional interpretation. The engine derives these directionalities from the beneficiary/victim structure and exit options; no overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The Manifesto suspends practice without renouncing doctrine, and the institutional authority structure formally committed to the principle that the suspension is temporary — awaiting future restoration when political circumstances allow. This is precisely the mandatrophy condition: the founding mandate (the eternal principle of plural marriage as required for exaltation) persists in the institution's official doctrine and ceremony (D&C 132 is canon; the principle is affirmed in temple ordinances), yet the practice is suppressed such that obedience to the institutional mandate is impossible. The classification as scaffold captures this: the constraint is justified by the transition (political accommodation), not by the steady state. The steady state (indefinite suppression) would be mandatrophy proper — a dead founding mandate held alive by theatrical affirmation while the practice remains permanently prohibited. The temporal_accommodation_reading asserts the constraint is genuinely temporary; the measurement data (theater_ratio rising and plateauing) suggest the theatrical performance has stabilized, which is consistent with either: (1) the constraint is successfully transient and awaiting restoration, or (2) the constraint has calcified into piton territory (theatrical maintenance of a dead mandate). The omegas document these ambiguities; the classification as scaffold expresses the reading's own endorsed interpretation (temporary suspension), not a claim that the data unambiguously supports it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_permanence_ambiguity,
    'Is the Manifesto''s suspension of plural marriage genuinely temporary (awaiting future restoration when political constraints lift) or permanently transformative (reclassifying polygamy from doctrine to historical practice)?',
    'Documentary evidence from institutional archives: leadership statements about restoration timeline; subsequent theological development affirming or abandoning the eternal-principle framing; pattern of doctrinal revival attempts or silence.',
    'If genuinely temporary (reading-endorsed claim), the constraint remains a transient suspension (scaffold). If permanently transformative (immutable_commandment_reading falsified), the classification shifts to snare — extraction continues indefinitely under cover of a doctrine that has become inert institutional theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suspension_permanence_ambiguity, empirical, 'Whether suspension is structurally temporary or a cover for permanent abandonment.').

omega_variable(
    doctrine_restoration_mechanism_absent,
    'Does the institutional authority structure retain a credible pathway to restore the suspended practice, or has the pathway eroded to the point where restoration is structurally impossible?',
    'Track institutional capacity (legal exposure, political capital) to propose plural marriage legalization; document leadership statements about future restoration; measure institutional investment in either maintaining or erasing the doctrinal apparatus.',
    'If the restoration pathway remains live, the suspension is a genuine temporary accommodation. If the pathway has atrophied, the constraint operates as inert theater (piton dynamics) — extraction persists through doctrinal ambiguity without the possibility of functional restoration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_restoration_mechanism_absent, empirical, 'Whether the institutional capacity to restore suspended practice persists.').

omega_variable(
    federal_coercion_vs_principled_accommodation,
    'Does the Manifesto represent a principled accommodation where institutional leadership freely chose temporal obedience over doctrine, or does it represent institutional capitulation under federal coercion (statehood threat, property seizure, prosecution)?',
    'Historical analysis of leadership deliberation records; comparison to voluntary doctrinal suspension elsewhere versus suspension under explicit threat; analysis of whether the suspension was the minimum required to achieve political goals or whether it went beyond necessity.',
    'If coerced, the effective extraction is higher — the constraint persists through imposed political pressure, not voluntary coordination. If principled, the constraint reflects a genuine value prioritization (temporal law above eternal doctrine). The reading''s core claim depends on the principled framing; coercion evidence would support the immutable_commandment_reading''s counter-claim that doctrine was abandoned under duress, not freely suspended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_coercion_vs_principled_accommodation, empirical, 'Whether the suspension reflects principled choice or institutional coercion.').

omega_variable(
    readings_semantic_stability,
    'Do the three sibling readings (immutable_commandment, prophetic_override, temporal_accommodation) represent genuinely distinct commitments, or do they collapse into one another when institutional actors shift positions under political pressure?',
    'Observe institutional leadership statements across political eras: if the same leaders assert immutable-commandment framing in one era and temporal-accommodation framing in another without acknowledging the shift, the readings are not robust categories but rhetorical tools. Document whether any reading''s foundational axiom has been formally repudiated or affirmed by the institution.',
    'If the readings are rhetorical rather than robust, the constraint architecture is theater — all three readings are ex-post-facto framings of the same underlying extraction (suppressing plural marriage under federal pressure). The classification would shift from scaffold to snare with high theater_ratio.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(readings_semantic_stability, conceptual, 'Whether the three kernel readings represent distinct commitments or rhetorical tools.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_pre_manifesto_1880, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(theater_pre_manifesto_1880, observed).
narrative_ontology:measurement(theater_manifesto_crisis_1890, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 2, 0.32).
narrative_ontology:measurement_basis(theater_manifesto_crisis_1890, observed).
narrative_ontology:measurement(theater_post_manifesto_1900, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 4, 0.51).
narrative_ontology:measurement_basis(theater_post_manifesto_1900, observed).
narrative_ontology:measurement(theater_statehood_1920, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 8, 0.58).
narrative_ontology:measurement_basis(theater_statehood_1920, observed).
narrative_ontology:measurement(theater_mid_century_1950, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 12, 0.6).
narrative_ontology:measurement_basis(theater_mid_century_1950, observed).
narrative_ontology:measurement(theater_contemporary_2000, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 16, 0.58).
narrative_ontology:measurement_basis(theater_contemporary_2000, observed).

% Extraction over time
narrative_ontology:measurement(extractiveness_pre_manifesto_1880, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(extractiveness_pre_manifesto_1880, observed).
narrative_ontology:measurement(extractiveness_manifesto_crisis_1890, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 2, 0.28).
narrative_ontology:measurement_basis(extractiveness_manifesto_crisis_1890, observed).
narrative_ontology:measurement(extractiveness_post_manifesto_1900, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement_basis(extractiveness_post_manifesto_1900, observed).
narrative_ontology:measurement(extractiveness_statehood_1920, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement_basis(extractiveness_statehood_1920, observed).
narrative_ontology:measurement(extractiveness_mid_century_1950, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement_basis(extractiveness_mid_century_1950, observed).
narrative_ontology:measurement(extractiveness_contemporary_2000, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement_basis(extractiveness_contemporary_2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(suppression_pre_manifesto_1880, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(suppression_pre_manifesto_1880, observed).
narrative_ontology:measurement(suppression_manifesto_crisis_1890, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 2, 0.62).
narrative_ontology:measurement_basis(suppression_manifesto_crisis_1890, observed).
narrative_ontology:measurement(suppression_post_manifesto_1900, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 4, 0.71).
narrative_ontology:measurement_basis(suppression_post_manifesto_1900, observed).
narrative_ontology:measurement(suppression_statehood_1920, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement_basis(suppression_statehood_1920, observed).
narrative_ontology:measurement(suppression_mid_century_1950, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement_basis(suppression_mid_century_1950, observed).
narrative_ontology:measurement(suppression_contemporary_2000, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(suppression_contemporary_2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__temporal_accommodation_reading, 0.12).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__prophetic_override_reading).

% DUAL FORMULATION NOTE:
% The eternal_marriage_covenant kernel admits three structurally distinct readings with different ε values, beneficiary/victim structures, and classifications. The immutable_commandment_reading models the doctrine as eternally binding and the Manifesto as institutional violation (high extraction, high suppression, snare-candidate). The prophetic_override_reading models the living prophet as having authority to supersede prior revelation (moderate extraction, enforcement mechanism as real as any theological precedent, rope-or-tangled-rope candidate). This temporal_accommodation_reading models the doctrine as suspended but not renounced, pending restoration when political constraints lift (moderate extraction, high theater as suppression persists without functional restoration pathway being visible, scaffold with piton-warning through omegas). All three readings share the referent (the historical Manifesto and the institutional accommodation that followed) but instantiate different constraints (different ε, different beneficiary/victim structures, different classifications) depending on whether the reader endorses immutability, prophetic authority, or temporal accommodation. Each reading is authored as a separate constraint story; the network links connect them as family members.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
