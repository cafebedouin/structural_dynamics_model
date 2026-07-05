% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   This story instantiates the temporal_accommodation_reading of the
 *   eternal_marriage_covenant kernel: the 1890 Manifesto is read as a
 *   suspension of practice under overwhelming federal pressure that leaves
 *   the underlying doctrine (plural/celestial marriage as an eternal
 *   principle per D&C 132) formally intact and dormant, pending a hoped-for
 *   future in which political constraints lift and the practice could in
 *   principle resume. This is structurally distinct from a reading that the
 *   doctrine was permanently and substantively renounced, and distinct from a
 *   reading that treats the change as prophetic override superseding prior
 *   revelation outright. The Manifesto's own language ('I hereby declare my
 *   intention to submit to those laws, and to use my influence with the
 *   members of the Church over which I preside to have them do likewise') was
 *   deliberately non-renunciatory — it addresses obedience to civil law, not
 *   the theological status of the underlying principle. Renewed and
 *   unauthorized plural marriages performed after 1890 (leading to the 1904
 *   'Second Manifesto') are read under this framing as evidence that even
 *   Church leadership initially treated the 1890 declaration as a political
 *   accommodation rather than doctrinal reversal — the dormancy, not
 *   renunciation, is the load-bearing structural claim this reading makes,
 *   and it is precisely what distinguishes it from its siblings.
 *
 * KEY AGENTS:
 *   - church_institutional_leadership
 *   - federal_government_of_the_united_states
 *   - plural_wives_and_children_of_suspended_marriages
 *   - fundamentalist_adherents_excommunicated_for_continuing_practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.42).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.58).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, scaffold).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "1890 Manifesto as Temporal Accommodation of Eternal Plural Marriage Doctrine").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).
narrative_ontology:has_sunset_clause(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, '9f8dc55a-affa-4833-bf6d-aee35e878def').
narrative_ontology:cs_kernel_codification('9f8dc55a-affa-4833-bf6d-aee35e878def', fixed_text).
narrative_ontology:cs_authority_grounding('9f8dc55a-affa-4833-bf6d-aee35e878def', lineage).
narrative_ontology:cs_interpretation_layer_present('9f8dc55a-affa-4833-bf6d-aee35e878def').
narrative_ontology:cs_reading_relation('9f8dc55a-affa-4833-bf6d-aee35e878def', eternal_marriage_covenant__immutable_commandment_reading, influences).
narrative_ontology:cs_reading_relation('9f8dc55a-affa-4833-bf6d-aee35e878def', eternal_marriage_covenant__prophetic_override_reading, coexists_with).
narrative_ontology:cs_axiom('9f8dc55a-affa-4833-bf6d-aee35e878def', foundational, doctrine_remains_valid_though_dormant).
narrative_ontology:cs_axiom_status(doctrine_remains_valid_though_dormant, holdable).
narrative_ontology:cs_axiom_grounding('9f8dc55a-affa-4833-bf6d-aee35e878def', doctrine_remains_valid_though_dormant, conventional).
narrative_ontology:cs_axiom('9f8dc55a-affa-4833-bf6d-aee35e878def', foundational, civil_obedience_takes_precedence_over_practice_without_altering_truth_status).
narrative_ontology:cs_axiom_status(civil_obedience_takes_precedence_over_practice_without_altering_truth_status, holdable).
narrative_ontology:cs_axiom_grounding('9f8dc55a-affa-4833-bf6d-aee35e878def', civil_obedience_takes_precedence_over_practice_without_altering_truth_status, instrumental).
narrative_ontology:cs_reference_frame('9f8dc55a-affa-4833-bf6d-aee35e878def', plural_marriage_as_active_eternal_commandment).
narrative_ontology:cs_drift_state('9f8dc55a-affa-4833-bf6d-aee35e878def', post_manifesto_institutional_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9f8dc55a-affa-4833-bf6d-aee35e878def', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, church_members_seeking_statehood_normalization).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, federal_government_of_the_united_states).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, plural_wives_and_children_of_suspended_marriages).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, fundamentalist_adherents_excommunicated_for_continuing_practice).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, women_in_unrecognized_post_manifesto_unions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, church_members_seeking_statehood_normalization).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, eternal_validity_of_celestial_marriage_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and administers the Manifesto, framing it as a suspension of practice rather than a renunciation of doctrine. Negotiates directly with federal authorities for statehood and amnesty, and retains discretion over how strictly the suspension is enforced against members. Preserves the eternal principle in doctrine (temple sealing language, D&C 132 remains canonical) while directing members to cease new plural marriages, giving leadership maximal flexibility to reinterpret the doctrine's operative status later.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_leadership, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_leadership, beneficiary).

% Sought cessation of plural marriage as a condition for Utah statehood and cessation of federal prosecution/asset seizure under the Edmunds-Tucker Act. Accepts the Manifesto's practical suspension as sufficient compliance without requiring doctrinal renunciation, extracting behavioral conformity while remaining formally indifferent to the Church's internal theological position.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, federal_government_of_the_united_states, beneficiary,
    institutional, generational, analytical, national).

% Gain relief from federal prosecution, restored civil rights, and a path to statehood and social normalization. Some among them held plural marriages themselves and must now navigate an ambiguous status where their existing unions are tolerated but new ones are forbidden — benefiting from the political settlement while absorbing personal and family disruption.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, church_members_seeking_statehood_normalization, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, church_members_seeking_statehood_normalization, payer).

% Existing plural wives and their children face an ambiguous legal and social status: their marriages are neither fully legitimated nor dissolved, inheritance and legal recognition remain unsettled, and they bear the reputational and material cost of a practice the institution now publicly disavows in effect while privately affirming in doctrine. They have no institutional voice in whether the suspension is temporary or permanent.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, plural_wives_and_children_of_suspended_marriages, payer,
    powerless, biographical, trapped, local).

% Take the doctrine's stated eternal validity literally and continue practicing plural marriage, reading the Manifesto as political expedience rather than genuine revelation. They are excommunicated, prosecuted, and stripped of institutional standing for acting on the very doctrine the institution never renounced — bearing the full cost of the gap between the Church's stated theology and its enforced practice.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, fundamentalist_adherents_excommunicated_for_continuing_practice, payer,
    powerless, generational, trapped, regional).

% Entered into plural marriages performed quietly after 1890 (some with tacit or explicit leadership sanction, e.g. the period addressed by the Second Manifesto of 1904) and were later left in a status the institution declined to formally acknowledge or defend, absorbing legal and social precarity that the ambiguity of 'suspension not renunciation' directly produced.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, women_in_unrecognized_post_manifesto_unions, payer,
    powerless, biographical, trapped, local).

% Study the documentary record, comparing public statements to private correspondence, post-Manifesto marriage records, and later doctrinal statements, to assess whether the 1890 declaration was genuine revelation, political survival strategy, or both. Their conclusions do not bind institutional policy but shape the historical record other readings draw on.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, historians_and_church_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_leadership).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__temporal_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for the institution to comply with overwhelming external legal and political pressure (federal prosecution, disincorporation threat, denial of statehood) without requiring members or leadership to repudiate the underlying theological claim, allowing continuity of core doctrine while altering observable behavior.
% TRANSFER_FUNCTION: Moves political liability and legal risk away from the institution and toward individual practitioners of plural marriage; moves civil legitimacy and property protection toward the institution and its mainstream membership; leaves existing plural families holding an unresolved status the institution declines to fully own or disown.
% ABSENT_VOICES: Existing plural wives, particularly those in marriages performed shortly before or quietly after 1890, had no seat in the negotiation between Church leadership and federal authorities; their consent to having their marital status become a diplomatic instrument was never sought. Fundamentalist adherents who took the doctrine's stated eternality at face value are also structurally absent from the decision that later excommunicated them for the same reading leadership itself endorsed in doctrine.
% DISAPPEARANCE_RATIONALE: If the Manifesto's temporal-accommodation framing were dropped entirely — either by reverting fully to open practice or by formally renouncing the eternal doctrine — the institution's political settlement with the federal government and its claim to unbroken prophetic continuity would both be destabilized. Mainstream Church members and leadership would say core continuity survives regardless (world_unchanged from their seat); fundamentalist claimants and historians would say the ambiguity itself is load-bearing and its removal would force a reckoning the institution has structurally deferred for over a century (world_rearranges from their seat).
% FOUNDING_PROBLEM: The Church faced disincorporation, mass property seizure under the Edmunds-Tucker Act, and permanent denial of Utah statehood unless plural marriage ceased; the Manifesto was built to solve the practical political-survival problem while preserving the doctrinal claim that celestial/plural marriage is an eternal principle revealed by God.
% FOUNDING_PROBLEM_CORROBORATION: Federal government records and contemporaneous press accounts (outside the Church's own institutional voice) corroborate that political and legal pressure, not independent theological reconsideration, precipitated the 1890 declaration. Independent historians examining post-1890 marriage records (including the 1904 Second Manifesto controversy) corroborate that the suspension was inconsistently enforced by the institution's own leadership for over a decade, undermining a purely doctrinal reading of the founding problem as fully resolved by 1890.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__temporal_accommodation_reading, contested).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__temporal_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.42) and declines then stabilizes: the heaviest institutional extraction (severed marriages, prosecuted families, contested inheritance) was concentrated in the immediate post-1890 decades, and settled into a lower steady-state extraction as the political crisis receded — but never fully vanishes because the doctrinal dormancy leaves affected families' status permanently unresolved rather than repaired. Theater ratio rises over the interval (0.40 to 0.61) because as the founding federal-pressure problem recedes, an increasing share of the constraint's operation becomes institutional performance of continuity (temple language, doctrinal statements affirming eternal validity) unconnected to any live practice — the theatrical maintenance of a dormant doctrine that structurally cannot be exercised under current civil and institutional policy. Suppression peaks around 1890-1904 (active excommunication and prosecution of continuing practitioners) and declines as fundamentalist practice was pushed out of the mainstream institution entirely, then ticks up slightly in the modern era as renewed public scrutiny (documentaries, lawsuits, splinter-group prosecutions) pressures the institution to actively police the boundary between accommodation and reversal.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting institutional seat, this reads as a scaffold: a genuine, functional transitional accommodation to an unsustainable federal conflict, with the doctrine held in trust rather than abandoned. From the payer seats — women in unresolved marriages, and especially fundamentalists later excommunicated for acting on the doctrine that was never formally renounced — the same structure computes closer to tangled rope or snare: real coordination benefit accrued to the institution and mainstream members, while the cost of the doctrinal ambiguity was concentrated on those with the least power to contest it. The engine's per-seat computation should surface this divergence rather than average it away.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership and the federal government sit near the beneficiary end: both extracted a stable political settlement from the Manifesto's ambiguity — the institution retained doctrinal continuity and negotiating leverage, the government obtained behavioral compliance without needing to litigate theology. Existing plural wives, their children, and fundamentalist adherents sit near the full-target end: trapped exit options, no voice in the negotiation, and the entire cost of the unresolved doctrinal status falls on them across generations. Mainstream members occupy an intermediate position — real beneficiaries of political normalization, but some carrying personal family disruption from the same policy.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification with a declared sunset condition (restoration if/when political constraints lift) prevents mislabeling this as either pure extraction (ignoring the genuine coordination benefit of avoiding federal disincorporation) or pure coordination (ignoring that the ambiguity was, and remains, structurally convenient for the institution regardless of whether the sunset condition is ever met). The founding_problem_status is authored as contested rather than dead precisely because the institution has never formally declared the sunset triggered — the doctrine remains dormant rather than resolved, which is the mandatrophy signature: a scaffold whose sunset clause has no operative mechanism for ever actually firing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manifesto_as_revelation_or_political_expedience,
    'Was the 1890 Manifesto a genuine revelatory event that changed the operative status of the doctrine, or a politically necessary declaration whose theological framing is a post-hoc accommodation device?',
    'Comparative analysis of contemporaneous private correspondence and diaries of Church leadership against the public declaration''s language, cross-referenced with the timeline of federal legal pressure (Edmunds-Tucker enforcement, Idaho test oath cases, threatened disincorporation) to establish whether doctrinal change preceded or followed the political crisis.',
    'If the declaration was substantially reactive to legal pressure rather than independently revelatory, the temporal_accommodation_reading''s dormancy claim is well-supported; if independent theological reconsideration is documented as preceding or independent of the legal crisis, the reading shifts toward genuine doctrinal evolution, weakening the distinction from the prophetic_override_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manifesto_as_revelation_or_political_expedience, empirical, 'Whether the Manifesto''s dormancy framing reflects genuine theological suspension or retrospective political rationalization.').

omega_variable(
    second_manifesto_consistency,
    'Does the need for a Second Manifesto in 1904 (after continued post-1890 plural marriages, some with apparent tacit approval from leadership) undermine the temporal_accommodation_reading''s claim that the 1890 declaration ever functioned as an actual, binding suspension?',
    'Review of documented post-1890 marriage authorizations, church court proceedings, and the 1904 declaration''s own language addressing the gap between 1890 and 1904.',
    'If the 1904 event shows the institution itself did not treat 1890 as a firm suspension, the temporal_accommodation_reading''s coherence weakens for the 1890-1904 window specifically, suggesting a later, harder suspension boundary should be the reading''s operative start date rather than 1890.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(second_manifesto_consistency, empirical, 'Whether the documented 14-year gap between manifestos is evidence against a clean 1890 suspension.').

omega_variable(
    sunset_condition_operability,
    'Is there any institutionally recognized mechanism or criterion by which the declared ''temporary'' suspension would ever be lifted, or is the dormancy permanent in practice despite being framed as conditional?',
    'Examination of subsequent official statements (post-1904) for any articulated conditions under which resumption would be considered, versus statements that treat the suspension as effectively permanent.',
    'If no operative sunset mechanism exists or has ever existed, the scaffold classification is itself contestable — a scaffold whose sunset clause can never fire functions structurally like a piton (a dormant doctrine maintained by institutional inertia and theatrical continuity-affirmation) rather than a genuine transitional support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_condition_operability, conceptual, 'Whether the accommodation''s declared temporariness is structurally real or a permanent dormancy dressed as provisional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 1890, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1890, 0.4).
narrative_ontology:measurement(eter_tr_t1904, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1904, 0.55).
narrative_ontology:measurement(eter_tr_t1930, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1930, 0.6).
narrative_ontology:measurement(eter_tr_t1960, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1960, 0.62).
narrative_ontology:measurement(eter_tr_t1990, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1990, 0.6).
narrative_ontology:measurement(eter_tr_t2026, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 2026, 0.61).

% Extraction over time
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(eter_be_t1904, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1904, 0.5).
narrative_ontology:measurement(eter_be_t1930, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1930, 0.45).
narrative_ontology:measurement(eter_be_t1960, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1960, 0.42).
narrative_ontology:measurement(eter_be_t1990, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(eter_be_t2026, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 2026, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1890, 0.7).
narrative_ontology:measurement(eter_su_t1904, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1904, 0.75).
narrative_ontology:measurement(eter_su_t1930, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1930, 0.65).
narrative_ontology:measurement(eter_su_t1960, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(eter_su_t1990, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(eter_su_t2026, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 2026, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, prophetic_override_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the eternal_marriage_covenant kernel. immutable_commandment_reading holds the doctrine as permanently binding and denies legitimate suspension occurred at all; prophetic_override_reading holds that continuing revelation genuinely superseded the prior commandment, ending its operative force rather than merely suspending it. This reading (temporal_accommodation_reading) occupies the structurally distinct middle position: doctrine unrevoked, practice suspended, resumption theoretically pending. Each reading has a different victim set — the immutable reading's victims are those who accept the Manifesto as authoritative and therefore forgo a commandment they're told is still binding; this reading's victims are those caught in the ambiguity of a suspension that is neither firm renunciation nor confirmed impermanence; the override reading's victims are those who continued the practice believing prior revelation still bound them against a leadership that now treats it as superseded.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
