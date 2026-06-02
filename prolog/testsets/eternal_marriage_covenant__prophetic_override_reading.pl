% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__prophetic_override_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: eternal_marriage_covenant__prophetic_override_reading
 *   human_readable: Eternal Marriage Covenant Under Prophetic Override: Authority to Suspend Practice via New Revelation
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   The eternal marriage covenant doctrine in a living-prophet religious
 *   tradition creates a fundamental structural tension: the covenant is
 *   framed as eternal and unchangeable, yet a living prophet claims authority
 *   to receive new revelation that overrides prior doctrine. This constraint
 *   models ONE reading—the prophetic override reading—which asserts that
 *   continuing revelation from the prophet constitutes legitimate authority
 *   to suspend or reinterpret covenantal practice. This reading is
 *   instantiated as a response to federal pressure and institutional survival
 *   crises. The prophetic override reading coexists with two sibling
 *   readings: (1) the immutable-commandment reading, which holds that the
 *   eternal covenant cannot be overridden by any subsequent revelation, and
 *   (2) the temporal-accommodation reading, which reframes the covenant as
 *   always having been conditional on cultural circumstances. This constraint
 *   story instantiates the prophetic override reading exclusively and routes
 *   the kernel ambiguity and sibling relationships through omega variables
 *   and cs_structure fields.
 *
 * KEY AGENTS:
 *   - Church Institutional Survival: Primary beneficiary (institutional/arbitrage) — receives prophetic authority as a mechanism to maintain institutional coherence and authority claims during crises
 *   - Prophetic Authority Structure: Beneficiary (institutional/arbitrage) — consolidates and expands the scope of prophetic authority; claims become self-justifying within the framework
 *   - Covenant Fidelity Believers: Primary victim (powerless/identity_locked) — identity constituted through eternal covenant doctrine; forced to absorb contradiction between 'eternal' and 'overridable'; structurally mobile but cannot exit without abandoning community identity
 *   - Female Practitioners: Secondary victim (moderate/constrained) — practice suspension has gendered impact; higher relational and authority costs; constrained exit by family/community/salvation claims
 *   - Doctrinal Consistency: Tertiary victim (analytical/trapped) — the abstract commitment to coherent doctrine cannot organize or advocate for itself; bears the cost of the contradiction
 *   - Doctrinal Reformulation Coalition: Organized secondary actor (organized/constrained) — scholars and reform-minded believers who see the override as temporary scaffold enabling doctrinal evolution; constrained participation in institutional evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.58).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.72).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Eternal Marriage Covenant Under Prophetic Override: Authority to Suspend Practice via New Revelation").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, '0b58d00e-704c-4d5b-8fb2-f6fdfe1f91f5').
narrative_ontology:cs_kernel_codification('0b58d00e-704c-4d5b-8fb2-f6fdfe1f91f5', fixed_text).
narrative_ontology:cs_authority_grounding('0b58d00e-704c-4d5b-8fb2-f6fdfe1f91f5', extraction).
narrative_ontology:cs_interpretation_layer_present('0b58d00e-704c-4d5b-8fb2-f6fdfe1f91f5').
narrative_ontology:cs_reading_relation('0b58d00e-704c-4d5b-8fb2-f6fdfe1f91f5', eternal_marriage_covenant__immutable_commandment_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b58d00e-704c-4d5b-8fb2-f6fdfe1f91f5', eternal_marriage_covenant__temporal_accommodation_reading, influences).
narrative_ontology:cs_axiom('0b58d00e-704c-4d5b-8fb2-f6fdfe1f91f5', foundational, continuing_revelation_overrides_prior_doctrine).
narrative_ontology:cs_axiom_status(continuing_revelation_overrides_prior_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('0b58d00e-704c-4d5b-8fb2-f6fdfe1f91f5', continuing_revelation_overrides_prior_doctrine, deontological).
narrative_ontology:cs_axiom('0b58d00e-704c-4d5b-8fb2-f6fdfe1f91f5', secondary, institutional_survival_activates_prophetic_authority).
narrative_ontology:cs_axiom_status(institutional_survival_activates_prophetic_authority, holdable).
narrative_ontology:cs_axiom_grounding('0b58d00e-704c-4d5b-8fb2-f6fdfe1f91f5', institutional_survival_activates_prophetic_authority, instrumental).
narrative_ontology:cs_reference_frame('0b58d00e-704c-4d5b-8fb2-f6fdfe1f91f5', prophet_as_ultimate_doctrinal_authority).
narrative_ontology:cs_drift_state('0b58d00e-704c-4d5b-8fb2-f6fdfe1f91f5', contemporary_federal_challenge_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0b58d00e-704c-4d5b-8fb2-f6fdfe1f91f5', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_institutional_survival).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, prophetic_authority_consolidation).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, covenant_fidelity_believers).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, female_practitioners).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, doctrinal_consistency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COVENANT FIDELITY BELIEVER (SNARE) — Identity constituted through eternal marriage covenant doctrine; structurally mobile (could physically leave) but identity-locked into the framework. Prophetic override creates extraction: believers must accept suspension of core practice through revelation mechanism they cannot challenge. No exit without abandoning community identity. Powerless before prophetic authority claim.
constraint_indexing:constraint_classification(eternal_marriage_covenant__prophetic_override_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: FEMALE COVENANT PARTICIPANTS (SNARE) — Bears disproportionate extraction: practice suspension affects gendered relationships and authority structures. High suppression (social stigma, community sanctions against exit). Constrained exit: leaving sacrifices family, community standing, salvation claims. Moderate power but severe extraction given gendered vulnerability.
constraint_indexing:constraint_classification(eternal_marriage_covenant__prophetic_override_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CHURCH INSTITUTIONAL ACTOR (TANGLED ROPE) — Receives prophetic override authority that enables survival under federal pressure. Genuine coordination function: manage institutional continuity amid doctrinal crisis. Extraction toward church: believers must absorb doctrinal contradiction while church maintains authority claim. Requires active enforcement (testimony, reframing, selective history). Arbitrage exit: can shift revelations or reinterpret as needed.
constraint_indexing:constraint_classification(eternal_marriage_covenant__prophetic_override_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROPHETIC AUTHORITY STRUCTURE (ROPE) — Sees constraint as pure coordination mechanism: new revelation solves the legitimacy crisis by providing authoritative resolution. Low experienced extraction because the authority structure generates the mechanism itself. Arbitrage exit: prophetic claims are self-justifying within the framework. Institutional power sustains the authority claim.
constraint_indexing:constraint_classification(eternal_marriage_covenant__prophetic_override_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DOCTRINAL REFORMULATION COALITION (SCAFFOLD) — Organized agents (scholars, theologians, reform-minded believers) see the override as temporary scaffolding: prophetic flexibility creates space for doctrinal evolution from eternal to conditional covenant. Sunset logic: as cultural norms shift, the need for prophetic override fades. Theater ≤ 0.70 because the reformulation work is genuinely generative. Constrained exit for coalitional agents: must maintain institutional participation while reframing from within.
constraint_indexing:constraint_classification(eternal_marriage_covenant__prophetic_override_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THEOLOGICAL LEGITIMACY RITUAL (PITON) — The apparatus of prophetic revelation is increasingly performative at civilizational scale. Modern observers recognize that 'new revelation' functions as institutional authorization mechanism rather than divine communication. Theatricality ≥ 0.70: the ritual persists through institutional inertia and identity fusion, not because it functions as claimed. Primary function has atrophied; form remains.
constraint_indexing:constraint_classification(eternal_marriage_covenant__prophetic_override_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL NATURAL LAW VIEW (MOUNTAIN) — Risk of false summit: the constraint is framed as immutable religious law—'God's revelation supersedes prior revelation'—but this naturalizes a contingent institutional arrangement. From civilizational scope, the prophetic override doctrine appears as an inherent feature of living religion. However, structural data reveals beneficiaries (church survival, prophetic authority), victims (believers forced to absorb contradiction), and active enforcement requirements. The engine's false summit detection will reclassify this as tangled rope or snare from analytical perspective, revealing that the naturalizing framing serves institutional interests.
constraint_indexing:constraint_classification(eternal_marriage_covenant__prophetic_override_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eternal_marriage_covenant__prophetic_override_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eternal_marriage_covenant__prophetic_override_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, TR),
    TR >= 0.70.

:- end_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The prophetic override doctrine extracts compliance from believers who must accept doctrinal contradiction without recourse. The extraction is not maximal (0.72+) because some institutional benefits accrue to believers (community, salvation claims, spiritual practice) even as they absorb the contradiction. The mechanism is pure authority assertion—'the prophet received new revelation'—which cannot be independently verified or challenged from within the framework. Suppression (0.72): High. Powerful suppressive mechanisms enforce acceptance: testimony rituals requiring public affirmation, social sanctions for doubt, theology of faith as transcending logic, identity fusion with community, and framing of resistance as faithlessness. The suppression has increased over the interval as federal pressure has intensified institutional defensiveness. Theater ratio (0.68): Moderate-high. The apparatus of prophetic revelation includes genuine theological work (reinterpreting prior doctrine in light of new revelation) but increasingly relies on performative elements: testimony rallies affirming the revelation, selective scripture emphasis, and framing dissent as Satanic opposition. The theater has risen as external scrutiny has increased, requiring more elaborate legitimacy work. Over 10 time units, theater increased from 0.45 to 0.68, indicating increasing performative overhead.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival gap across the observation site. The covenant fidelity believer (powerless/identity_locked) experiences snare classification—pure extraction cloaked in sacred language, with no legitimate exit. The female practitioner (moderate/constrained) experiences snare classification—extraction with gendered asymmetry. The church institution (institutional/arbitrage) experiences tangled rope—genuine coordination (institutional survival) with extraction that runs toward the institution. The prophetic authority structure (institutional/arbitrage) experiences rope classification—pure coordination of believers with divine will. The doctrinal reformation coalition (organized/constrained) experiences scaffold classification—temporary mechanism enabling evolutionary change. The theological legitimacy ritual (institutional/arbitrage) appears as piton—performative apparatus maintained through inertia. The civilizational analytical observer risks false-summit mountain classification (revelation is inherent to living religion) until the structural data reveals beneficiaries and enforcement requirements. The gap between snare (victim experience) and rope (beneficiary experience) is the core signal that extraction is occurring and being naturalized through sacred language.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position relative to the extraction flow: beneficiaries with arbitrage options (d ≈ 0.05-0.20) experience low or negative effective extractiveness; victims with identity-locked or trapped exit (d ≈ 0.85-0.95) experience maximum effective extractiveness. The church institution receives prophetic authority as a beneficiary (d ≈ 0.15), producing rope classification and low chi. The powerless believer is a victim with identity-lock (d ≈ 0.89), producing snare classification and high chi. The moderate female practitioner is a victim with constrained exit (d ≈ 0.75), producing snare classification and moderate-high chi. The organized coalition has moderate power and constrained exit (d ≈ 0.45), producing scaffold classification. No directionality overrides are needed—the derivation chain captures the structural relationships without amendment.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy at the theological level: is the prophetic override a legitimate mechanism for adjusting eternal doctrine (rope/tangled_rope reading) or a mechanism for extracting believer compliance while suppressing doctrinal consistency (snare reading)? The resolution requires cross-positional analysis: the beneficiary's rope classification and the victim's snare classification cannot both be true of the same structure—one is misclassifying extraction as coordination or coordination as extraction. The mandatrophy is resolved by recognizing that both are partially true: the prophetic override IS a genuine coordination mechanism (church survival requires institutional coherence) AND an extraction mechanism (believers must absorb contradiction at identity cost). The tangled rope classification at the church institutional perspective captures this: the constraint has both coordination function AND asymmetric extraction. The snare classification at the victim perspective captures that from the victim's standpoint, the coordination benefit does not accrue to them—they experience pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_revelation_vs_institutional_survival_mechanism,
    'Is the prophetic override a genuine divine communication mechanism or an institutional survival strategy that uses revelation language for legitimacy?',
    'Comparative analysis: correlation between federal pressure timeline and revelation announcements; documentation of prior internal dissent preceding revelations; analysis of selective scriptural emphasis preceding override claims',
    'If divine communication: constraint is rope (pure coordination of believers with divine will). If institutional survival strategy: constraint is snare or tangled_rope (extraction mechanism cloaked in sacred language). Classification gap is the core analytical signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_revelation_vs_institutional_survival_mechanism, empirical, 'Whether prophetic override is divine communication or institutional survival mechanism').

omega_variable(
    believer_perception_of_contradiction,
    'Can believers genuinely perceive the prophetic override as non-contradictory to prior covenant claims, or is the contradiction suppressed through cognitive dissonance management?',
    'Ethnographic analysis of believer testimony; documentation of framing strategies used to reconcile contradiction; measurement of identity-lock persistence (do believers who leave report identity reorientation or experience it as core loss?)',
    'If contradiction is perceived: identity_locked classification is accurate (agents know but cannot act on the contradiction). If contradiction is cognitively suppressed: exit mechanism is even more constrained; doctrine functions through epistemic closure rather than explicit reframing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(believer_perception_of_contradiction, empirical, 'Whether believers perceive doctrinal contradiction or suppress it cognitively').

omega_variable(
    alternative_authorization_structures_availability,
    'Did the church have non-prophetic institutional mechanisms available to resolve the covenant-practice contradiction, and if so, why was the prophetic override mechanism chosen instead?',
    'Historical analysis of internal deliberation; comparison with peer institutions that resolved similar crises via council, scholarship, or practice reinterpretation without prophetic claims; documentation of deliberate choice to use prophetic authority',
    'If alternatives existed: choice to use prophetic override reveals extraction motive (maintain concentrated authority rather than distribute legitimacy-grounding). If no alternatives: constraint approaches rope (genuine coordination solution). The choice reveals whether suppression is structural or enforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_authorization_structures_availability, empirical, 'Whether non-prophetic authorization alternatives were available and rejected').

omega_variable(
    reading_kernel_ambiguity,
    'What is the kernel—the stabilized commitment—that this reading interprets? Is it ''eternal marriage covenant'' as a fixed doctrine, or ''continuing revelation as authorizing mechanism'' as the true kernel?',
    'Doctrinal archaeology: trace which commitment the institution prioritizes when conflicts arise between them. If eternal marriage doctrine is revised while prophetic authority is defended, prophetic authority is the kernel. If prophetic authority is limited while marriage is defended, marriage is the kernel.',
    'If marriage is the kernel: this reading forecloses the immutable_commandment_reading. If prophetic authority is the kernel: this reading coexists with or influences both siblings. The resolution determines the logical topology of the sibling relationships.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether the kernel is eternal marriage doctrine or continuing revelation authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emc_prop_tr_t0, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(emc_prop_tr_t5, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(emc_prop_tr_t10, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(emc_prop_be_t0, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(emc_prop_be_t5, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(emc_prop_be_t10, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(emc_prop_su_t0, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(emc_prop_su_t5, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(emc_prop_su_t10, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__prophetic_override_reading, 0.12).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__temporal_accommodation_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, prophetic_authority_legitimacy_claim).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, federal_pressure_institutional_survival).

% DUAL FORMULATION NOTE:
% The eternal marriage covenant kernel has three structurally distinct constraint readings with different epsilon values: prophetic_override_reading (ε=0.58, Tangled Rope), immutable_commandment_reading (ε≈0.25, Mountain), and temporal_accommodation_reading (ε≈0.30, Rope). Each reading models a different interpretation of the covenant's immutability and the source of legitimate authority. The three readings are linked via network.affects_constraints to show the constraint family structure. The prophetic override reading is downstream of institutional survival pressure and influences the other two readings by asserting prophetic authority as the arbiter of doctrinal validity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
