% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__originalist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: us_constitution_interpretive__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Interpretation (Fixed Meaning at Ratification)
 *   domain: legal/political
 *
 * SUMMARY:
 *   This story authors ONE reading of the constitutional-interpretation
 *   kernel: the originalist reading, under which constitutional meaning was
 *   fixed at ratification and interpretive authority derives from fidelity to
 *   the framers' intent or, in its contemporary form, the original public
 *   meaning of the enacted text. The arrangement has a genuine coordination
 *   function — a single publicly ascertainable decision procedure that
 *   disciplines judicial discretion and anchors official action in enacted
 *   consent — and a real asymmetric cost structure: entire classes of claims
 *   (rights the ratified text never named, federal regulatory programs beyond
 *   the 1787 grant) have no discovery channel inside it. That combination is
 *   why the claimed type is tangled_rope rather than rope or snare. The claim
 *   and the metrics are independent authored facts: the metrics describe the
 *   arrangement's actual operation as the originalist reading constitutes it,
 *   and the engine computes per-seat classifications from the structural
 *   data. Interval mapping: T=0 corresponds to 1981 (originalism's emergence
 *   as an organized movement and executive-branch doctrine), T=45 to 2026.
 *   KEY AGENTS (by structural relationship): see key_agents; the receipt
 *   surface names originalist_judicial_majority as the seat where the
 *   arrangement's gains — concentrated interpretive authority and doctrinal
 *   control — demonstrably accrue.
 *
 * KEY AGENTS:
 *   - originalist_judicial_majority: Agenda-setter (institutional/identity_locked) — administers the fixed-meaning method on the federal bench; collects interpretive authority and doctrinal control
 *   - unenumerated_rights_claimants: Primary target (powerless/trapped) — bear foreclosed rights recognition; no discovery channel inside the method
 *   - federal_regulatory_expansion_advocates: Secondary target (institutional/constrained) — blocked from federal regulatory growth by the fixed enumeration despite substantial global standing
 *   - federalism_advocates: Primary beneficiary (powerful/mobile) — win doctrinal ground whenever federal power reads narrowly
 *   - property_rights_defenders: Beneficiary (powerful/mobile) — takings and economic-liberty claims fare better under founding-era understandings
 *   - religious_liberty_claimants: Conditional beneficiary (organized/constrained) — protected when claims fit founding-era categories, exposed otherwise
 *   - living_constitutionalist_jurists: Excluded (institutional/mobile) — increasingly absent from the appellate seats where the method contest is decided
 *   - article_v_amendment_actors: Excluded (organized/constrained) — hold the formal escape valve the reading leaves nearly unreachable
 *   - legal_historians: Analytical observer (analytical/analytical) — produce the founding-era evidence the method consumes; see the full structure including selective use
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, 0.58).
domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, 0.62).
domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "Originalist Reading of Constitutional Interpretation (Fixed Meaning at Ratification)").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, '787bccb8-7c19-4039-8cd9-e4097a33e5d0').
narrative_ontology:cs_kernel_codification('787bccb8-7c19-4039-8cd9-e4097a33e5d0', fixed_text).
narrative_ontology:cs_authority_grounding('787bccb8-7c19-4039-8cd9-e4097a33e5d0', lineage).
narrative_ontology:cs_interpretation_layer_present('787bccb8-7c19-4039-8cd9-e4097a33e5d0').
narrative_ontology:cs_reading_relation('787bccb8-7c19-4039-8cd9-e4097a33e5d0', us_constitution_interpretive__living_constitution_reading, forecloses).
narrative_ontology:cs_reading_relation('787bccb8-7c19-4039-8cd9-e4097a33e5d0', us_constitution_interpretive__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('787bccb8-7c19-4039-8cd9-e4097a33e5d0', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('787bccb8-7c19-4039-8cd9-e4097a33e5d0', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('787bccb8-7c19-4039-8cd9-e4097a33e5d0', secondary, judicial_updating_authority_illegitimate).
narrative_ontology:cs_axiom_status(judicial_updating_authority_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('787bccb8-7c19-4039-8cd9-e4097a33e5d0', judicial_updating_authority_illegitimate, deontological).
narrative_ontology:cs_axiom('787bccb8-7c19-4039-8cd9-e4097a33e5d0', secondary, framers_subjective_intent_binding).
narrative_ontology:cs_axiom_status(framers_subjective_intent_binding, overridden).
narrative_ontology:cs_axiom_grounding('787bccb8-7c19-4039-8cd9-e4097a33e5d0', framers_subjective_intent_binding, empirically_contingent).
narrative_ontology:cs_reference_frame('787bccb8-7c19-4039-8cd9-e4097a33e5d0', ratification_era_public_meaning_baseline).
narrative_ontology:cs_drift_state('787bccb8-7c19-4039-8cd9-e4097a33e5d0', contemporary_dominance_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('787bccb8-7c19-4039-8cd9-e4097a33e5d0', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, religious_liberty_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, original_public_meaning_methodology).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, enumerated_powers_federalism).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, countermajoritarian_legitimacy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits on the federal bench committed to deciding constitutional questions by the text's public meaning at ratification. Controls which doctrines survive, which precedents are overruled, and which interpretive tests govern the lower courts. Their authority depends on the method staying ascendant, and their professional identities and legacies are bound to it; leaving would mean repudiating a lifetime of jurisprudential work.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, originalist_judicial_majority, agenda_setter,
    institutional, generational, identity_locked, national).

% State governments, state-court systems, and the political coalitions that defend state autonomy. Win durable doctrinal ground whenever federal power is read narrowly against the 1787 grant: police-power regulation, firearms policy, and welfare design stay home. Can pursue their goals through state legislatures and state courts regardless of federal doctrinal weather, which gives them usable outside options.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federalism_advocates, beneficiary,
    powerful, generational, mobile, national).

% Owners, developers, and business coalitions whose takings, contract, and economic-liberty claims fare better under founding-era understandings of property than under later readings tolerant of expansive regulation. They can structure holdings, choose jurisdictions, and litigate selectively, so adverse doctrine is a cost rather than a wall.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, property_rights_defenders, beneficiary,
    powerful, biographical, mobile, national).

% Religious individuals and institutions seeking accommodation. Claims that fit founding-era categories — established-church objections, traditional forms of exercise — tend to succeed; claims resting on post-founding understandings of conscience or equality fare worse. Their position is conditional: protected when the claim resembles the founding baseline, exposed when it does not.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, religious_liberty_claimants, beneficiary,
    organized, biographical, constrained, national).

% People asserting rights the ratified text does not name — bodily autonomy, intimate association, dignity-based protections. The fixed-meaning method offers them no discovery channel: the right was never enacted, so no interpretive effort can find it. Remedies lie outside this framework entirely — ordinary legislation, state constitutions, or an amendment threshold almost never reached. Individually they hold little leverage; their numbers are large but diffuse, and coalition formation is slow against a lifetime-tenured bench.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Administrative agencies, congressional majorities pursuing national solutions, and the legal movements that staff them. The fixed enumeration caps what federal power can reach no matter the problem's scale; their program advances only through the narrow corridors the 1787 grant leaves open or through amendment. Statutory workarounds and state-level policy remain available but are costly substitutes for the instrument this arrangement closes.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates, payer,
    institutional, generational, constrained, national).

% Judges and former judges formed in the adaptation-to-contemporary-values method. Increasingly absent from the appellate seats where the method contest is decided; they publish, dissent from the margins, teach, and wait for future appointments. Their exclusion from the deciding seats is what the current composition maintains.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, living_constitutionalist_jurists, excluded,
    institutional, biographical, mobile, national).

% Movements and coalitions attempting to change the constitution's content through its formal mechanisms. The reading directs all change-energy toward an amendment threshold requiring supermajorities that a small bloc of states can block — a door that has opened rarely in practice. They hold the only sanctioned escape from fixed meaning and can barely reach its handle.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, article_v_amendment_actors, excluded,
    organized, generational, constrained, national).

% Scholars producing the founding-era evidence — period dictionaries, usage corpora, ratification debates — that the method consumes. They observe the full structure: which historical claims get used, which get ignored, and how selective the selection is. They neither collect nor pay inside the adjudicative game.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__originalist_reading, originalist_judicial_majority).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a single, publicly ascertainable decision procedure for constitutional disputes: the semantic content of the ratified text settles what government may do, so litigants, officials, and citizens can predict outcomes without each interpreter importing contemporary moral judgment; it disciplines judicial discretion and anchors official action in an enacted consent.
% TRANSFER_FUNCTION: Moves interpretive authority and doctrinal protection: decision power over contested social questions flows from federal institutions and rights-asserting litigants toward state governments and holders of historically recognized entitlements; protection flows away from claims the ratified text never named toward claims it did.
% ABSENT_VOICES: The ratification baseline was fixed without the voices of enslaved people, women, and Indigenous nations — the populations whose descendants now litigate hardest against it; they are absent from the founding consent the method treats as binding. In the present, living-constitutionalist jurists are largely absent from the appellate seats where the method contest is decided. Both groups sit outside the conversation that produced the constraint's terms.
% DISAPPEARANCE_RATIONALE: If the fixed-meaning requirement vanished overnight, every open constitutional question would re-enter play under whatever method the next majority brought: administrative deference, firearm regulation, religious accommodations, unenumerated-rights protections, and federalism boundaries would all rearrange within a few appointment cycles; the state-federal power allocation would shift toward Washington; and the bar's argument practice would rebuild around adaptive tools.
% FOUNDING_PROBLEM: Unelected judges deciding momentous social questions under color of interpreting a text they were in fact updating — the countermajoritarian difficulty sharpened by the mid-century rights revolution. Originalism was organized as a discipline on judicial discretion and a legitimacy account: judges apply law the people enacted; they do not govern.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: political-science work on the countermajoritarian difficulty and public-opinion research on judicial legitimacy — largely from non-originalist scholars — corroborate that the legitimacy problem is real; the same literature disputes whether fixed meaning cures it rather than relocating it. No source outside the movement attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_interpretive__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__originalist_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is substantial but not cover-story extraction: the coordination function is real and conceded even by many victims, yet the same structure systematically forecloses whole claimant classes, which keeps epsilon well above rope territory. Suppression (0.62) reflects the enforcement machinery — appointment screening, confirmation politics, doctrinal overruling campaigns — that keeps rival readings off the bench while leaving them alive in academia; suppression is authored as a raw structural property and is NOT scaled by power or scope (only extractiveness is scaled, by directionality and scope, in the engine). Theater (0.35) is moderate: core historical method is genuine, but a growing share of 'history and tradition' deployment is selective costume serving predetermined outcomes. Accessibility_collapse (0.35) is low because alternatives persist — state constitutions, ordinary legislation, comparative practice, the theoretical amendment route. Resistance (0.70) is high: sustained scholarly critique, litigant challenge, and confirmation warfare. The measurement series run on ONE shared time grid (T=0,9,18,27,36,45) with every tracked metric authored at every point. Trajectories: base_extractiveness rises as the movement captured the bench and unwound adversarial precedent; suppression_requirement rises with confirmation screening and overruling intensity (enforcement ratchet, not decay); theater_ratio rises as selective historicism grows with dominance. End-state values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat the arrangement is legitimacy-restoring discipline: judges stop governing and start applying law the people enacted. From the trapped target seat the same structure is a closed door with a historical plaque on it — the right was never enacted, so no amount of interpretive effort finds it. Beneficiary seats experience restored federal balance and secured entitlements; the excluded jurist seats experience a method war lost by attrition. The engine computes these divergent per-seat classifications from power, exit, and role data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Victim declarations drive the target end: unenumerated_rights_claimants (powerless, trapped) sit nearest full-target — effective extraction amplifies hardest where exit is foreclosed; federal_regulatory_expansion_advocates are institutionally powerful in general but structurally targeted by THIS constraint specifically, so their directionality stays high despite global standing — a reminder that d encodes the relationship to this constraint, not general clout. Beneficiaries with mobile exits (federalism_advocates, property_rights_defenders) sit near the subsidy end, damping or inverting their effective extraction. religious_liberty_claimants occupy a conditional middle — benefited when claims resemble the founding baseline, exposed otherwise; the derivation places them low-d as declared beneficiaries, and NO directionality override is authored because overrides key on power atoms and would misfire on other organized seats sharing the atom; the conditionality is documented here and in the ratification_baseline_exclusion omega instead. National spatial scope modestly amplifies verification-adjusted extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the countermajoritarian difficulty) is contested rather than dead, and overnight disappearance would rearrange the constitutional order — the status-times-verdict pair shows a live-disputed mandate, not a zombie one, so no dead-mandate mismatch flag fires. The classification guards both error directions: labeling this a snare would erase the genuine coordination function (determinate law, anchored consent) that even adversely-situated parties partially concede; labeling it a rope would erase the systematic foreclosure borne by claimant classes the founding baseline never contemplated. The rising theater series is the leading indicator to watch: if historical method becomes fully selective costume while outcomes track coalition interest, the coordination half atrophies and the hybrid tips toward pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates only the originalist_reading of the us_constitution_interpretive kernel; how would the classification shift under the sibling readings?',
    'Author the sibling stories (us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive__popular_constitutionalism_reading) and compare per-seat classifications across the family.',
    'Under the living reading the victim and beneficiary sets largely invert (federal regulatory advocates become beneficiaries; unenumerated rights claimants gain a discovery channel); under popular constitutionalism the agenda-setting seat migrates from judges to political movements, changing the receipt surface.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this constraint is one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    intent_public_meaning_split,
    'Are ''fidelity to framers'' intent'' and ''fidelity to original public meaning'' one constraint or two? The variants diverge wherever subjective founding purpose and enacted semantic content part ways.',
    'Track the movement''s operative standard: if adjudication reverts to subjective-intent evidence (drafting history, private correspondence) versus enacted public meaning (period dictionaries, usage corpora), split into two stories per the epsilon-invariance rule.',
    'Contemporary practice has converged on public meaning, stabilizing this story''s epsilon; a subjective-intent resurgence would raise theater (undiscoverable evidence invites selective citation) and shift victim sets toward groups disadvantaged by founder-specific purposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_public_meaning_split, conceptual, 'Internal variant split within the reading; candidate decomposition if the variants'' outcomes diverge again.').

omega_variable(
    fixed_meaning_naturalness,
    'Is fixed semantic anchoring a structural feature of any durable codified constitution, or a constructed political choice adopted because it benefits identifiable coalitions?',
    'Comparative constitutional analysis: whether long-lived written constitutions universally develop fixed-meaning interpretive anchors, and whether anchor strength tracks beneficiary-coalition strength rather than textual age.',
    'If structural, part of the measured extraction is irreducible coordination cost of written law; if constructed, the extraction is fully attributable to this reading''s adoption and the classification sits deeper in hybrid territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fixed_meaning_naturalness, conceptual, 'Naturality ambiguity: near-mountain component of codified-law authority versus constructed advantage.').

omega_variable(
    enforcement_locus_contingency,
    'Does the reading''s persistence depend on contingent appointment events (retirements, deaths, election timing) or on durable professional socialization of the bar and bench?',
    'Natural experiment across appointment shocks: whether doctrine reverts when Court composition flips, or whether lower-court hiring, clerkship networks, and law-school curricula entrench the method regardless of who sits.',
    'If contingent, the suppression_requirement series should oscillate with Court composition and reversal risk is high; if socialized, suppression hardens independently of composition and the enforcement ratchet is structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_locus_contingency, empirical, 'Whether the enforcement trajectory is appointment-luck or professional entrenchment.').

omega_variable(
    ratification_baseline_exclusion,
    'The ratifying constituency excluded enslaved people, women, and Indigenous nations; does the legitimacy anchor — consent given at ratification — survive as foundation for those populations'' descendants?',
    'Legitimacy survey data across descendant communities and comparative analysis of constitutional orders that re-founded consent through amendment or wholesale replacement.',
    'If the anchor fails for these classes, the coordination function degrades toward enforced hierarchy for them specifically, pushing their seats'' computed types toward pure extraction even while the general structure retains coordination character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratification_baseline_exclusion, conceptual, 'Whether the consent anchor underlying the coordination claim holds for populations excluded from ratification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__originalist_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t9, us_constitution_interpretive__originalist_reading, theater_ratio, 9, 0.2).
narrative_ontology:measurement_basis(us_c_tr_t9, observed).
narrative_ontology:measurement(us_c_tr_t18, us_constitution_interpretive__originalist_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement_basis(us_c_tr_t18, observed).
narrative_ontology:measurement(us_c_tr_t27, us_constitution_interpretive__originalist_reading, theater_ratio, 27, 0.28).
narrative_ontology:measurement_basis(us_c_tr_t27, observed).
narrative_ontology:measurement(us_c_tr_t36, us_constitution_interpretive__originalist_reading, theater_ratio, 36, 0.32).
narrative_ontology:measurement_basis(us_c_tr_t36, observed).
narrative_ontology:measurement(us_c_tr_t45, us_constitution_interpretive__originalist_reading, theater_ratio, 45, 0.35).
narrative_ontology:measurement_basis(us_c_tr_t45, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__originalist_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t9, us_constitution_interpretive__originalist_reading, base_extractiveness, 9, 0.39).
narrative_ontology:measurement_basis(us_c_be_t9, observed).
narrative_ontology:measurement(us_c_be_t18, us_constitution_interpretive__originalist_reading, base_extractiveness, 18, 0.44).
narrative_ontology:measurement_basis(us_c_be_t18, observed).
narrative_ontology:measurement(us_c_be_t27, us_constitution_interpretive__originalist_reading, base_extractiveness, 27, 0.49).
narrative_ontology:measurement_basis(us_c_be_t27, observed).
narrative_ontology:measurement(us_c_be_t36, us_constitution_interpretive__originalist_reading, base_extractiveness, 36, 0.54).
narrative_ontology:measurement_basis(us_c_be_t36, observed).
narrative_ontology:measurement(us_c_be_t45, us_constitution_interpretive__originalist_reading, base_extractiveness, 45, 0.58).
narrative_ontology:measurement_basis(us_c_be_t45, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__originalist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t9, us_constitution_interpretive__originalist_reading, suppression_requirement, 9, 0.45).
narrative_ontology:measurement_basis(us_c_su_t9, observed).
narrative_ontology:measurement(us_c_su_t18, us_constitution_interpretive__originalist_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement_basis(us_c_su_t18, observed).
narrative_ontology:measurement(us_c_su_t27, us_constitution_interpretive__originalist_reading, suppression_requirement, 27, 0.54).
narrative_ontology:measurement_basis(us_c_su_t27, observed).
narrative_ontology:measurement(us_c_su_t36, us_constitution_interpretive__originalist_reading, suppression_requirement, 36, 0.58).
narrative_ontology:measurement_basis(us_c_su_t36, observed).
narrative_ontology:measurement(us_c_su_t45, us_constitution_interpretive__originalist_reading, suppression_requirement, 45, 0.62).
narrative_ontology:measurement_basis(us_c_su_t45, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__living_constitution_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% One colloquial concept — 'how the Constitution is interpreted' — decomposes into three structurally distinct constraints (readings of the us_constitution_interpretive kernel): fixed meaning (this story), evolving meaning, and popularly contested meaning. Each carries its own epsilon, beneficiary/victim structure, and classification; they are linked rather than merged because measuring interpretation through the fixed-meaning lens yields a different epsilon than through the adaptive lens. This reading exerts structural pressure on both siblings: its capture of the federal bench pushes living-constitutionalist adjudication toward state courts and raises the practical price of the popular-contestation route, without logically eliminating either as a held position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
