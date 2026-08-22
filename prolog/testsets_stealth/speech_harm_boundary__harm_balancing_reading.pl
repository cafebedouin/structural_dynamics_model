% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__harm_balancing_reading, []).

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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Speech Protection Presumptive but Yielding to Demonstrated Harm (Harm-Balancing Reading)
 *   domain: constitutional law/political philosophy/communication ethics
 *
 * SUMMARY:
 *   In jurisdictions that adopt this framework, expression enjoys a legal
 *   presumption of freedom, and the state — and, through horizontal
 *   application, private actors and platforms — may restrict it only upon a
 *   showing of concrete harm to others, with every restriction tested for
 *   legitimate aim, suitability, necessity, and overall balance. The
 *   framework emerged from the post-war constitutional settlement; the 1966
 *   ICCPR Article 19(3) formula is its clearest codification, and it now
 *   governs hate-speech statutes, harassment law, group-vilification
 *   offenses, and platform-era speech regulation across most constitutional
 *   democracies. Operationally it coordinates a predictable boundary between
 *   liberty and protection while distributing its costs unevenly: speakers
 *   whose expression loses at balancing bear sanctions and channel closure,
 *   targets whose injuries fail the evidentiary threshold absorb continued
 *   harm without remedy, and the adjudicating institutions accumulate
 *   jurisdiction with each accepted category. Interval mapping: T0
 *   corresponds to 1966 (codification of the limitation formula); T60
 *   corresponds to the present (2026). The claimed type and the authored
 *   metrics are independent facts: the story claims tangled_rope and authors
 *   the descriptive metrics separately, without tuning either to the other.
 *
 * KEY AGENTS:
 *   - - constitutional_courts: Agenda-setter (institutional/constrained) — administers the balancing framework and captures adjudicative authority with each accepted category
 *   - - national_legislatures: Agenda-setter with beneficiary tilt (institutional/constrained) — enacts the category-defining statutes and collects validated regulatory space
 *   - - targets_of_demonstrated_harm: Primary beneficiary (moderate/identity_locked) — collects enforceable remedies when harm clears the evidentiary threshold
 *   - - ordinary_political_speakers: Diffuse beneficiary (moderate/mobile) — speaks under the working presumption, pays diffuse chilling costs
 *   - - press_publishers: Dual-positioned beneficiary/payer (organized/mobile) — defends the presumption, absorbs edge-case liability
 *   - - restricted_speakers: Primary payer (moderate/constrained) — bears sanctions, records, and channel closure
 *   - - sub_threshold_harm_targets: Payer with incidental beneficiary role (powerless/identity_locked) — absorbs lawful harm without remedy
 *   - - underresourced_claimants: Excluded voice (powerless/trapped) — priced out of the evidentiary process that defines the line
 *   - - international_human_rights_monitors: Analytical observer (institutional/analytical) — audits national applications across jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.48).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.54).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Speech Protection Presumptive but Yielding to Demonstrated Harm (Harm-Balancing Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional law/political philosophy/communication ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, 'f1baaa53-9c1f-41bc-94e9-a4fb576775c4').
narrative_ontology:cs_kernel_codification('f1baaa53-9c1f-41bc-94e9-a4fb576775c4', fixed_text).
narrative_ontology:cs_authority_grounding('f1baaa53-9c1f-41bc-94e9-a4fb576775c4', lineage).
narrative_ontology:cs_interpretation_layer_present('f1baaa53-9c1f-41bc-94e9-a4fb576775c4').
narrative_ontology:cs_reading_relation('f1baaa53-9c1f-41bc-94e9-a4fb576775c4', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1baaa53-9c1f-41bc-94e9-a4fb576775c4', speech_harm_boundary__dignity_reading, influences).
narrative_ontology:cs_axiom('f1baaa53-9c1f-41bc-94e9-a4fb576775c4', foundational, expression_protection_yields_to_demonstrated_harm).
narrative_ontology:cs_axiom_status(expression_protection_yields_to_demonstrated_harm, holdable).
narrative_ontology:cs_axiom_grounding('f1baaa53-9c1f-41bc-94e9-a4fb576775c4', expression_protection_yields_to_demonstrated_harm, empirically_contingent).
narrative_ontology:cs_axiom('f1baaa53-9c1f-41bc-94e9-a4fb576775c4', foundational, restrictions_proportionate_to_legitimate_aims).
narrative_ontology:cs_axiom_status(restrictions_proportionate_to_legitimate_aims, holdable).
narrative_ontology:cs_axiom_grounding('f1baaa53-9c1f-41bc-94e9-a4fb576775c4', restrictions_proportionate_to_legitimate_aims, instrumental).
narrative_ontology:cs_reference_frame('f1baaa53-9c1f-41bc-94e9-a4fb576775c4', presumptive_expression_liberty_harm_override).
narrative_ontology:cs_drift_state('f1baaa53-9c1f-41bc-94e9-a4fb576775c4', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f1baaa53-9c1f-41bc-94e9-a4fb576775c4', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, targets_of_demonstrated_harm).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, ordinary_political_speakers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, constitutional_courts).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, national_legislatures).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, restricted_speakers).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, sub_threshold_harm_targets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, press_publishers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, sub_threshold_harm_targets).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, press_publishers).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, proportionality_review_doctrine).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, millian_harm_principle_application).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide which expressions fall outside protection and which restrictions survive review. They define what counts as demonstrated harm, which aims are legitimate, and whether a given measure is necessary and proportionate. Every category they accept adds to the docket and widens the scope of their review; they are bound by constitutional text, precedent, and treaty obligations, but they control the operative meaning of each term.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Enact the statutes that create regulated categories — group vilification offenses, harassment provisions, election-speech rules — and set penalty levels. When courts uphold their statutes they gain validated regulatory space; when courts strike them down they must redraft. Electoral incentives favor visible action against salient harms.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, national_legislatures, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__harm_balancing_reading, national_legislatures, beneficiary).

% People and communities whose ethnic, religious, or other identity characteristics make them the object of hostile speech — harassment victims, vilified minorities. When they can document concrete injury they obtain remedies: takedowns, fines, damages, criminal referral. Their exposure follows from who they are, not from anything they chose, and they cannot step out of the characteristic that draws the speech. Access to remedy tracks their evidentiary and legal resources.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, targets_of_demonstrated_harm, beneficiary,
    moderate, biographical, identity_locked, national).

% Citizens, commentators, and advocates whose expression concerns public affairs and almost never approaches a regulated category. They speak under a working presumption of protection and rarely encounter the machinery; their main cost is uncertainty about where the boundaries sit, which occasionally leads them to soften lawful expression.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, ordinary_political_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Newsrooms and platforms of record. Core reporting sits safely inside the presumption, but investigative and conflict coverage brushes group-libel and harassment lines, exposing them to complaints and occasional liability. They defend the presumption vigorously while absorbing edge-case costs.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, press_publishers, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__harm_balancing_reading, press_publishers, payer).

% Speakers whose expression lost at some stage of review — convicted under vilification statutes, fined for harassing posts, enjoined from repeating statements. They bear sanctions, records, and the practical closure of domestic channels. Some reroute expression through foreign platforms or pseudonyms; open attributed speech at home is largely closed to them.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, restricted_speakers, payer,
    moderate, biographical, constrained, national).

% People who suffer real injury from speech that reviewers classify as offensive, distasteful, or insufficiently demonstrable as harm — diffuse disparagement, demeaning stereotype, coordinated ridicule below the legal line. They receive no remedy and continue to absorb the conduct; the same presumption that denies them protection also shields their own speech.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, sub_threshold_harm_targets, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__harm_balancing_reading, sub_threshold_harm_targets, beneficiary).

% Would-be complainants who lack the money, time, or legal knowledge to carry an evidentiary burden through review. Their cases are never filed, so their experience of where the line falls never enters the record that shapes where the line sits.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, underresourced_claimants, excluded,
    powerless, biographical, trapped, national).

% Supranational courts and treaty bodies that audit national applications of the framework, publish comparative assessments, and occasionally reverse national outcomes. They see the whole surface across jurisdictions and hold no enforcement power of their own beyond reputational and treaty pressure.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, international_human_rights_monitors, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__harm_balancing_reading, constitutional_courts).
narrative_ontology:fixing_cost_class(speech_harm_boundary__harm_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, adjudicable boundary between expressive liberty and protection from demonstrable harm, replacing raw power contests over speech with structured review: legislatures gain a lawful path to address documented injuries while a presumption shields ordinary discourse from majoritarian suppression.
% TRANSFER_FUNCTION: Moves expressive freedom and sanction risk from speakers in regulated categories toward targets with demonstrated claims; moves adjudicative authority, caseload, and doctrinal territory toward the reviewing institutions; moves certainty away from boundary-adjacent speakers.
% ABSENT_VOICES: Sub-threshold claimants without litigation resources, anonymous low-power online speakers whose cases never shape the categories, and communities in jurisdictions where the framework has not been adopted would all object to aspects of the current settlement; none holds a seat in the proceedings that define the categories.
% DISAPPEARANCE_RATIONALE: Pending prosecutions and tribunal dockets would collapse; enacted vilification and harassment statutes would become unenforceable; targets of demonstrated harm would lose their remedy path; the reviewing institutions would lose a principal domain of review; legislatures would lose validated regulatory space. The speech economy of adopting jurisdictions is organized around the framework's categories — removal forces immediate rearrangement, not equilibrium.
% FOUNDING_PROBLEM: Post-war constitutional settlement faced a double failure: democracies that had tolerated persecution-adjacent mobilization until it destroyed them, and regimes that suppressed all dissent in the name of order. The framework was built to protect the discourse democracy needs while permitting restraint of incitement, vilification, and harassment — reconciling democratic speech with the preconditions of democratic personhood.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: mid-century drafting histories of the ICCPR limitation clause and post-war basic laws attest the founding problem; defense-bar litigation and civil-liberties scholarship — parties that lose under the current categories — attest the problem remains live while disputing where the line sits. No beneficiary-only attestation is relied on.
narrative_ontology:disappearance_verdict(speech_harm_boundary__harm_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__harm_balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__harm_balancing_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end): the presumption keeps the bulk of expression free, but category breadth has grown steadily — group vilification, harassment, and platform-era duties layered onto the original incitement and libel core — imposing real restriction costs on a widening band, while sub-threshold targets collect nothing. Suppression (0.54) is authored as a raw structural property, unscaled by power or scope: the framework runs on active machinery (criminal statutes, tribunal orders, platform compliance duties), bounded by the demonstration requirement and due process. Theater ratio (0.29) is low-to-moderate — most activity is functional adjudication, with a growing symbolic share (performative prosecutions, compliance signaling). Accessibility_collapse is low (0.28): rival boundary frameworks remain visibly operational in peer jurisdictions, so understanding this framework does not close off alternatives. Resistance is moderate (0.45): speaker coalitions, press interests, and losing litigants contest categories continuously. The temporal series run on one shared grid (T0-T60 at decade steps) so every tracked metric is authored at every examined point; the rising suppression_requirement series is authored deliberately because the story specifically tracks enforcement-capacity buildout (specialized prosecution units, tribunal growth, platform duty regimes), not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the bench the framework appears as craft: doctrine refined case by case, each category a reasoned response to demonstrated injury. From a restricted speaker's position the same structure is a sanctioning machine entered at the state's initiative. From a sub-threshold target's position it is a locked door: the more rigorously the evidentiary standard is applied, the more certainly their injury stays lawful. Same-level divergence: two speakers of identical formal standing occupy opposite positions depending on proximity to a regulated category — the ordinary commentator experiences only the presumption, while the identity-adjacent critic experiences the full apparatus; global power does not explain the difference, category adjacency does. Coalition dynamics: dispersed payers rarely coordinate — sanctioned speakers are stigmatized individually and sub-threshold targets are numerous but unorganized — so payer-side power stays low despite headcount, which is precisely what keeps the extraction side of the hybrid stable.
 *
 * DIRECTIONALITY LOGIC:
 *   targets_of_demonstrated_harm sits nearest the beneficiary pole: remedies flow to it, and its exposure is constitutive rather than chosen. constitutional_courts and national_legislatures derive low-to-moderate d: they collect authority and validated regulatory space rather than bearing costs. ordinary_political_speakers sit near symmetric with a beneficiary tilt — presumption received, diffuse chilling paid. press_publishers sit slightly past symmetric, reflecting the dual role. restricted_speakers derive high d — direct sanctions with constrained exit. sub_threshold_harm_targets derive the highest d in the story: they pay (unremedied harm) without collecting, and identity-lock amplifies their position toward the full-target end. The beneficiary/victim declarations map onto these relationships directly; no directionality override was needed because exit-option variation already separates the seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling democratic discourse with restraint of persecution-adjacent speech — remains live, so the mandate has not outlived its function and no sunset applies. Classification discipline cuts both ways: reading the arrangement as pure extraction ignores the real coordination service (a shared, adjudicable liberty/protection boundary that replaces raw power contests with structured review); reading it as pure coordination ignores the measurable asymmetries (category breadth falls hardest on dissident and minority speakers, sub-threshold targets receive nothing, adjudicative authority compounds with each accepted category). The tangled_rope claim preserves both halves. The temporal signature — extractiveness and enforcement intensity rising together on a stable coordination core — is the pattern the engine should test as hybrid operation rather than terminal drift; the category-ratchet omega marks the open question that would distinguish correctable hybrid from accumulating extraction infrastructure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading — the harm_balancing_reading — of the speech_harm_boundary kernel; how would instantiating a sibling reading change the structure?',
    'Comparative institutional analysis: substitute the absolutist sibling''s near-absolute threshold or the dignity sibling''s categorical bans and re-derive the beneficiary/victim sets and epsilon. The disagreement is located in the height of the harm-override threshold and in whether categories are treated categorically or case-by-case balanced.',
    'The absolutist sibling shrinks the unprotected set and shifts extraction toward targets of unchecked harmful speech; the dignity sibling broadens categorical bans and shifts extraction toward speakers. Either substitution changes this story''s victim arrays and likely its computed type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega: kernel membership, sibling structural deltas, and the located disagreement (override-threshold height; categorical versus balanced structure).').

omega_variable(
    demonstrated_harm_evidentiary_standard,
    'What quantum and kind of proof counts as demonstrated harm — individual psychological injury, group-level vilification effects, contribution to systemic discrimination?',
    'Longitudinal studies of targeted cohorts combined with systematic coding of review outcomes across jurisdictions.',
    'A demanding standard narrows effective categories and pushes the arrangement toward pure coordination; a lax standard that conflates harm with offense widens categories and pushes toward extraction-dominant operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstrated_harm_evidentiary_standard, empirical, 'The evidentiary content of the harm threshold is unsettled and drives effective category breadth.').

omega_variable(
    structural_vs_internalized_chilling,
    'How much of the suppression observed near the boundary is structural (statutes, sanctions, platform duties) versus internalized (self-censorship habits that persist where enforcement is weak)?',
    'Natural experiments where enforcement lapses or decriminalizes: if cautious behavior persists after sanctions recede, a large internalized component exists.',
    'If largely internalized, true suppression exceeds the formal scalar and the framework restrains more expression than its enforcement record shows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_chilling, empirical, 'Split of boundary-region suppression between external enforcement and internalized caution.').

omega_variable(
    balancing_discretion_direction,
    'Does proportionality review systematically favor public-order and state aims over speaker interests, or distribute outcomes even-handedly?',
    'Outcome coding of a large case sample controlling for category and jurisdiction.',
    'Systematic state-favoring would concentrate extraction on dissident and minority speakers and signal drift toward extraction dominance; even-handedness supports the hybrid coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_discretion_direction, empirical, 'Direction of discretionary lean in the balancing methodology.').

omega_variable(
    category_ratchet_reversibility,
    'Can an accepted unprotected category ever contract, or does acceptance ratchet irreversibly?',
    'Historical scan for category repeal or judicial narrowing that survives appeal, versus persistent re-enactment after strikes.',
    'Irreversibility converts each category into permanent extraction infrastructure and predicts drift toward a persistence-by-inertia profile; reversibility keeps the arrangement correctable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_ratchet_reversibility, conceptual, 'Whether category expansion is reversible or ratcheted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shb_hbr_tr_t0, speech_harm_boundary__harm_balancing_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(shb_hbr_tr_t0, observed).
narrative_ontology:measurement(shb_hbr_tr_t10, speech_harm_boundary__harm_balancing_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(shb_hbr_tr_t10, observed).
narrative_ontology:measurement(shb_hbr_tr_t20, speech_harm_boundary__harm_balancing_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(shb_hbr_tr_t20, observed).
narrative_ontology:measurement(shb_hbr_tr_t30, speech_harm_boundary__harm_balancing_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement_basis(shb_hbr_tr_t30, observed).
narrative_ontology:measurement(shb_hbr_tr_t40, speech_harm_boundary__harm_balancing_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement_basis(shb_hbr_tr_t40, observed).
narrative_ontology:measurement(shb_hbr_tr_t50, speech_harm_boundary__harm_balancing_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement_basis(shb_hbr_tr_t50, observed).
narrative_ontology:measurement(shb_hbr_tr_t60, speech_harm_boundary__harm_balancing_reading, theater_ratio, 60, 0.29).
narrative_ontology:measurement_basis(shb_hbr_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(shb_hbr_be_t0, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 0, 0.33).
narrative_ontology:measurement_basis(shb_hbr_be_t0, observed).
narrative_ontology:measurement(shb_hbr_be_t10, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement_basis(shb_hbr_be_t10, observed).
narrative_ontology:measurement(shb_hbr_be_t20, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement_basis(shb_hbr_be_t20, observed).
narrative_ontology:measurement(shb_hbr_be_t30, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement_basis(shb_hbr_be_t30, observed).
narrative_ontology:measurement(shb_hbr_be_t40, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement_basis(shb_hbr_be_t40, observed).
narrative_ontology:measurement(shb_hbr_be_t50, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 50, 0.47).
narrative_ontology:measurement_basis(shb_hbr_be_t50, observed).
narrative_ontology:measurement(shb_hbr_be_t60, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 60, 0.48).
narrative_ontology:measurement_basis(shb_hbr_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(shb_hbr_su_t0, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0, 0.36).
narrative_ontology:measurement_basis(shb_hbr_su_t0, observed).
narrative_ontology:measurement(shb_hbr_su_t10, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(shb_hbr_su_t10, observed).
narrative_ontology:measurement(shb_hbr_su_t20, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement_basis(shb_hbr_su_t20, observed).
narrative_ontology:measurement(shb_hbr_su_t30, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 30, 0.47).
narrative_ontology:measurement_basis(shb_hbr_su_t30, observed).
narrative_ontology:measurement(shb_hbr_su_t40, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement_basis(shb_hbr_su_t40, observed).
narrative_ontology:measurement(shb_hbr_su_t50, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement_basis(shb_hbr_su_t50, observed).
narrative_ontology:measurement(shb_hbr_su_t60, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 60, 0.54).
narrative_ontology:measurement_basis(shb_hbr_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'speech protection' covers at least three structurally distinct arrangements differing in override-threshold height, category breadth, and victim sets. Per the epsilon-invariance principle the label is decomposed into one story per reading; this file is the harm-balancing member. Siblings: speech_harm_boundary__absolutist_reading and speech_harm_boundary__dignity_reading. The balancing reading functions as the methodological center of the family — its proportionality apparatus is the terrain on which the other two readings argue — so influence edges run from this story to both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
