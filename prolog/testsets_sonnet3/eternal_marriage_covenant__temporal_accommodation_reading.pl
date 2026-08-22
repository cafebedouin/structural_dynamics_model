% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: 1890 Manifesto as Temporal Accommodation of Eternal Plural Marriage Doctrine
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   This story instantiates the temporal-accommodation reading of the eternal
 *   marriage covenant kernel: the 1890 Manifesto is read as a scaffold — a
 *   declared suspension of plural-marriage practice under acute federal legal
 *   pressure, explicitly not a doctrinal repudiation. The eternal principle
 *   of D&C 132 is treated as remaining true but dormant pending a future
 *   political relaxation that would permit restoration. This is a distinct
 *   constraint from the immutable-commandment reading (which holds the
 *   doctrine as presently binding and unsuspendable) and the
 *   prophetic-override reading (which holds that the living prophet's
 *   authority to issue new revelation simply supersedes and effectively ends
 *   the prior one). All three readings share the same underlying kernel text
 *   and history but commit to different structural claims about what changed
 *   in 1890 and why. Ambiguity was itself functional: it let the institution
 *   negotiate statehood without either alienating members loyal to the
 *   eternal doctrine or provoking further federal punishment for open
 *   defiance.
 *
 * KEY AGENTS:
 *   - church_institutional_leadership: administers the suspension, negotiates with the federal government, preserves the doctrine's validity claim
 *   - existing_plural_wives and children_of_plural_marriages: bear the practical cost of a status neither fully legitimized nor repudiated
 *   - members_taught_doctrine_as_eternal_and_binding: hold a belief the institution will not disavow but no longer permits acting on
 *   - federal_government: applies the pressure that forces the suspension and collects the policy outcome it sought
 *   - fundamentalist_successor_groups: later excluded for acting on the doctrine the institution itself preserved as valid
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.52).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.61).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, scaffold).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "1890 Manifesto as Temporal Accommodation of Eternal Plural Marriage Doctrine").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).
narrative_ontology:has_sunset_clause(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, '97656374-21bc-4f2d-a16e-3593263b4b2e').
narrative_ontology:cs_kernel_codification('97656374-21bc-4f2d-a16e-3593263b4b2e', formalized).
narrative_ontology:cs_authority_grounding('97656374-21bc-4f2d-a16e-3593263b4b2e', lineage).
narrative_ontology:cs_interpretation_layer_present('97656374-21bc-4f2d-a16e-3593263b4b2e').
narrative_ontology:cs_reading_relation('97656374-21bc-4f2d-a16e-3593263b4b2e', eternal_marriage_covenant__immutable_commandment_reading, coexists_with).
narrative_ontology:cs_reading_relation('97656374-21bc-4f2d-a16e-3593263b4b2e', eternal_marriage_covenant__prophetic_override_reading, influences).
narrative_ontology:cs_axiom('97656374-21bc-4f2d-a16e-3593263b4b2e', foundational, doctrinal_validity_survives_practical_suspension).
narrative_ontology:cs_axiom_status(doctrinal_validity_survives_practical_suspension, holdable).
narrative_ontology:cs_axiom_grounding('97656374-21bc-4f2d-a16e-3593263b4b2e', doctrinal_validity_survives_practical_suspension, conventional).
narrative_ontology:cs_axiom('97656374-21bc-4f2d-a16e-3593263b4b2e', foundational, obedience_to_civil_law_takes_precedence_over_temporal_practice_of_eternal_principle).
narrative_ontology:cs_axiom_status(obedience_to_civil_law_takes_precedence_over_temporal_practice_of_eternal_principle, holdable).
narrative_ontology:cs_axiom_grounding('97656374-21bc-4f2d-a16e-3593263b4b2e', obedience_to_civil_law_takes_precedence_over_temporal_practice_of_eternal_principle, instrumental).
narrative_ontology:cs_reference_frame('97656374-21bc-4f2d-a16e-3593263b4b2e', continuous_unbroken_revelation_authority).
narrative_ontology:cs_drift_state('97656374-21bc-4f2d-a16e-3593263b4b2e', post_statehood_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('97656374-21bc-4f2d-a16e-3593263b4b2e', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, church_members_seeking_statehood_integration).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, monogamous_second_generation_members).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, existing_plural_wives).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, children_of_plural_marriages).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, members_taught_doctrine_as_eternal_and_binding).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, federal_government).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, eternal_law_supremacy_over_temporal_practice).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, obedience_to_law_of_land_as_scriptural_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and administers the Manifesto as an announcement of suspension rather than repudiation, preserving the doctrinal claim in D&C 132 while directing members to cease contracting new plural marriages. Negotiates statehood, amnesty, and property restoration with federal authorities on the strength of the suspension, while privately signaling to some members that the underlying principle remains true and dormant.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_leadership, beneficiary).

% Remain married under a covenant the institution will not renounce but can no longer publicly defend or fully provide for. Existing marriages are neither dissolved nor fully legitimized going forward; many lose inheritance clarity, legal standing, and social protection as the institution manages its relationship to federal law around them rather than for them.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, existing_plural_wives, payer,
    powerless, biographical, trapped, regional).

% Inherit contested legitimacy status, complicated inheritance and legal recognition, and a doctrinal narrative that frames their family structure as eternally true but now practically abandoned. They have no voice in the suspension decision and no clear path to either full vindication or clean severance from the practice.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, children_of_plural_marriages, payer,
    powerless, biographical, trapped, regional).

% Built testimony, family structure, and eternal salvation expectations around plural marriage as required doctrine. The Manifesto asks for behavioral compliance without theological retraction, leaving them holding a belief the institution says is still true but no longer to be acted upon — a cognitive position that is costly to hold and costly to abandon.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, members_taught_doctrine_as_eternal_and_binding, payer,
    moderate, generational, identity_locked, regional).

% Grow up after the suspension with a simplified, monogamous practice, statehood-integrated citizenship, reduced federal antagonism, and the option to treat the doctrine as historical rather than operative. They benefit from the institutional accommodation without bearing the costs paid by the transitional generation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, monogamous_second_generation_members, beneficiary,
    moderate, generational, mobile, national).

% Applies escalating legal and property-confiscation pressure (Edmunds-Tucker Act and predecessors) forcing the suspension. Achieves its policy objective of ending institutionalized polygamy in the territory and clears the path to statehood, without needing the Church to theologically recant.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, federal_government, beneficiary).

% Read the doctrine's continued validity as license to continue the practice after the institution abandons it, and are subsequently excommunicated and marginalized by the same institution that never formally renounced the principle they act on. Their claim to doctrinal continuity is treated as illegitimate by the body that authored the doctrine's persistence.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, fundamentalist_successor_groups, excluded,
    powerless, civilizational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_leadership).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__temporal_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for the institution to comply with escalating federal legal pressure and secure territorial statehood and property restoration, while avoiding an internal crisis of prophetic authority that a full doctrinal repudiation would trigger.
% TRANSFER_FUNCTION: Moves legal risk, property jeopardy, and political isolation away from the institution and its leadership and onto individual plural families, who absorb the practical costs of a suspension the institution frames as obedience to law rather than as a change of belief.
% ABSENT_VOICES: Existing plural wives and their children had no seat in drafting the Manifesto's language and no mechanism to contest the ambiguous status it left them in; fundamentalist successor groups who took the preserved doctrine at face value were later excluded from the very institution that preserved it for them to act on.
% DISAPPEARANCE_RATIONALE: If the temporal-accommodation framing disappeared and the doctrine were either fully repudiated or fully reinstated, the institution's negotiated relationship with the federal government, its property holdings, its path to statehood, and its internal claim to continuous unbroken revelation would all have to be renegotiated from a different footing — the ambiguity itself is load-bearing.
% FOUNDING_PROBLEM: The Church faced federal seizure of Church property, disincorporation, disenfranchisement of members, and an indefinite bar to territorial statehood because of institutionalized plural marriage; the Manifesto was built to relieve that pressure while preserving the claim that the 1843 revelation had been true and remained true.
% FOUNDING_PROBLEM_CORROBORATION: Federal officials and historians outside the Church attest the political pressure was real and the Manifesto functioned as capitulation to it; mainstream institutional leadership attests the doctrine was never renounced, only its practice suspended; fundamentalist successor groups — themselves benefiting from neither reading — corroborate from outside current institutional interest that the doctrine was preserved as valid at the time of suspension, citing the Manifesto's own language and contemporaneous private instruction.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__temporal_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__temporal_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end): the constraint's primary cost is not financial extraction but the transfer of unresolved legal, familial, and testimonial risk onto individuals who cannot obtain closure in either direction. Theater ratio rises across the interval (0.35 to 0.58) as the gap between public suspension and preserved-but-unspoken doctrinal validity widens — the accommodation increasingly performs compliance for federal and public audiences while the private doctrinal position persists largely unchanged, which is the classic scaffold-to-piton risk profile for the temporal-accommodation reading specifically (a story the immutable-commandment and prophetic-override readings would not tell this way, since one denies any performance and the other denies any residual validity). Suppression is high and slightly declining (0.70 to 0.61) as federal pressure that necessitated the original enforcement eases with statehood achieved in 1896, though enforcement against those who continued the practice persisted.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership's seat, the Manifesto is a successful act of obedience to law of the land that preserved doctrinal integrity intact — a coordination success. From the seat of existing plural families, the same instrument is an extraction of legal and social protection without a corresponding resolution of their theological or practical status. The engine's per-seat computation should reflect this: agenda_setter/beneficiary seats will likely compute nearer coordination, trapped/identity_locked payer seats nearer extraction, from the identical structural facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Church institutional leadership sits at the beneficiary end: it retains doctrinal authority, resolves the federal crisis, and secures statehood and property restoration, at low structural cost to itself. Existing plural wives, their children, and members who internalized the doctrine as eternal and binding sit at the target end: they are structurally trapped or identity-locked by commitments made under one doctrinal regime and asked to live under a different practical regime without their belief structure being validated or corrected. Monogamous second-generation members are genuine beneficiaries of the accommodation with none of the transitional costs — this generational asymmetry is central to why the scaffold reading, not the mountain or rope reading, fits: a scaffold's justification is the transition, and the people who pay for the transition are not the people who inherit its benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold's declared sunset — restoration when political constraints lift — never definitively arrives; the founding federal-pressure problem substantially resolved (Utah achieved statehood in 1896, most federal antagonism subsided by 1904), yet the doctrine remains formally preserved as valid rather than either restored or repudiated. This is exactly the mismatch the founding-problem/disappearance-verdict cross-check exists to catch: founding_problem_status is contested precisely because the institution's own account (problem persists in modified form) diverges from external corroboration (the specific federal pressure that necessitated the 1890 announcement was substantially resolved by the 1904 Second Manifesto, which hardened the suspension rather than lifting it) — suggesting the scaffold's temporary character was itself provisional, with the practical suspension outliving the political conditions that were its stated justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_dormancy_vs_permanent_abandonment,
    'Is the doctrine''s ''eternal validity, temporarily suspended'' status a genuine holding pattern awaiting future restoration, or a permanent abandonment dressed in language designed to avoid the institutional cost of formal repudiation?',
    'Track whether the institution ever issues a formal doctrinal statement either restoring the practice or explicitly repudiating D&C 132''s binding force; absence of either after more than a century is itself evidence bearing on the question. Compare private leadership communications from the suspension period (where available) against the public Manifesto language.',
    'If the institution never resolves the dormancy in either direction, the scaffold''s own declared sunset condition (restoration when political constraints lift) becomes permanently unmet, converting the classification pressure toward piton — a suspended function maintained by inertia and unwillingness to pay the cost of formal resolution rather than active temporary coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_dormancy_vs_permanent_abandonment, empirical, 'Whether the temporal accommodation is a genuine transitional scaffold or a permanent piton dressed as temporary.').

omega_variable(
    kernel_reading_indeterminacy,
    'Among the three readings of the eternal marriage covenant kernel (immutable_commandment, prophetic_override, temporal_accommodation), which one the 1890 Manifesto''s authors actually intended is itself contested and may be irreducibly so, since the text was deliberately drafted to be compatible with multiple readings for negotiating audiences.',
    'Comparative textual and historical analysis of the Manifesto''s drafting process, contemporaneous private correspondence among Church leadership, and the specific audiences (federal officials vs. Church membership) the ambiguous language was crafted to satisfy.',
    'If the drafting evidence supports one reading over the others as the authors'' actual intent, that reading gains stronger claim to being the ''true'' structural account and the other two become better characterized as later retrospective reconstructions rather than contemporaneous alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the three kernel readings represent a genuine contemporaneous ambiguity or later retrospective disagreement.').

omega_variable(
    beneficiary_status_of_second_generation,
    'Are monogamous second-generation members genuine beneficiaries of the accommodation, or do they simply inherit a settlement whose costs were already paid by the prior generation, making their ''benefit'' merely the absence of continued extraction rather than a positive gain?',
    'Compare material and social outcomes (property security, legal standing, social acceptance) for second-generation members against a counterfactual where the practice had either been fully restored or fully repudiated in 1890.',
    'If their position is better characterized as absence-of-harm rather than active benefit, the beneficiary declaration should be reconsidered, which would shift the directionality computation for that stakeholder group closer to symmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_status_of_second_generation, conceptual, 'Whether second-generation members are true beneficiaries or merely non-victims of a resolved cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1890, 0.35).
narrative_ontology:measurement(eter_tr_t1892, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1892, 0.42).
narrative_ontology:measurement(eter_tr_t1896, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1896, 0.48).
narrative_ontology:measurement(eter_tr_t1898, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1898, 0.52).
narrative_ontology:measurement(eter_tr_t1901, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1901, 0.55).
narrative_ontology:measurement(eter_tr_t1904, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1904, 0.58).

% Extraction over time
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1890, 0.38).
narrative_ontology:measurement(eter_be_t1892, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1892, 0.43).
narrative_ontology:measurement(eter_be_t1896, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1896, 0.47).
narrative_ontology:measurement(eter_be_t1898, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1898, 0.5).
narrative_ontology:measurement(eter_be_t1901, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1901, 0.49).
narrative_ontology:measurement(eter_be_t1904, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1904, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1890, 0.7).
narrative_ontology:measurement(eter_su_t1892, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1892, 0.66).
narrative_ontology:measurement(eter_su_t1896, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1896, 0.62).
narrative_ontology:measurement(eter_su_t1898, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1898, 0.6).
narrative_ontology:measurement(eter_su_t1901, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1901, 0.61).
narrative_ontology:measurement(eter_su_t1904, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1904, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__temporal_accommodation_reading, 0.1).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__prophetic_override_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the eternal_marriage_covenant kernel. immutable_commandment_reading treats the doctrine as presently binding and treats any 'suspension' framing as illegitimate — it would author near-maximal extraction against the institution itself for abandoning a commandment it claims cannot be abandoned. prophetic_override_reading treats continuing revelation as having simply and cleanly ended the prior commandment's force — it would author near-zero residual doctrinal extraction since nothing remains dormant. This reading (temporal_accommodation) occupies the middle structural position: validity preserved, practice suspended, resolution deferred — which is what generates its distinctive theater-ratio drift and its scaffold-to-piton risk profile that the other two readings do not share by construction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
