% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__conceptual_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__conceptual_emergence_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: statute_of_anne_ip_foundation__conceptual_emergence_reading
 *   human_readable: Statute of Anne as Conceptual Emergence: Copyright as a Limited Regulatory Tool for Learning
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the statute_of_anne_ip_foundation
 *   kernel: the claim that the 1710 statute created a new conceptual space in
 *   which copyright exists as a limited regulatory tool for learning rather
 *   than as perpetual property. The epsilon referent is the standing
 *   statutory arrangement under contest - limited terms, registration, and
 *   enforced exclusivity - assessed by this reading's own lights; the
 *   endorsed alternative (whatever regime critics prefer) is not the
 *   referent. The sibling readings (institutional_reallocation_reading,
 *   entangled_event_reading) are separate constraint files; their deltas are
 *   routed to omegas, not averaged into this story. KEY AGENTS (by structural
 *   relationship): - parliament_legislators: Agenda setter
 *   (institutional/mobile) - enacted and administers the limited-term bargain
 *   - stationers_perpetual_rights_holders: Declared victim of the
 *   reallocation and principal collector of term rents
 *   (organized/constrained) - working_authors: Beneficiary with assignment
 *   discount (moderate/constrained) - reading_public: Beneficiary during and
 *   after terms (powerless/constrained) - future_generations_of_readers: Pure
 *   downstream beneficiary (powerless/civilizational) -
 *   provincial_pirate_printers: Excluded competitor (moderate/mobile) -
 *   courts_of_law: Analytical observer (institutional/analytical). The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as a
 *   transitional instrument with an intrinsic sunset, while the metrics are
 *   authored descriptively from the historical record; the engine computes
 *   per-seat classifications and any divergence is the datum.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.34).
domain_priors:suppression_score(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.38).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.17).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0.17).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__conceptual_emergence_reading, scaffold).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__conceptual_emergence_reading, "Statute of Anne as Conceptual Emergence: Copyright as a Limited Regulatory Tool for Learning").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__conceptual_emergence_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:has_sunset_clause(statute_of_anne_ip_foundation__conceptual_emergence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__conceptual_emergence_reading, '389bde7e-69fe-4946-8f30-9c07a50f7ce4').
narrative_ontology:cs_kernel_codification('389bde7e-69fe-4946-8f30-9c07a50f7ce4', formalized).
narrative_ontology:cs_authority_grounding('389bde7e-69fe-4946-8f30-9c07a50f7ce4', lineage).
narrative_ontology:cs_interpretation_layer_present('389bde7e-69fe-4946-8f30-9c07a50f7ce4').
narrative_ontology:cs_reading_relation('389bde7e-69fe-4946-8f30-9c07a50f7ce4', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_reading_relation('389bde7e-69fe-4946-8f30-9c07a50f7ce4', statute_of_anne_ip_foundation__entangled_event_reading, forecloses).
narrative_ontology:cs_axiom('389bde7e-69fe-4946-8f30-9c07a50f7ce4', foundational, copyright_created_as_limited_statutory_category).
narrative_ontology:cs_axiom_status(copyright_created_as_limited_statutory_category, holdable).
narrative_ontology:cs_axiom_grounding('389bde7e-69fe-4946-8f30-9c07a50f7ce4', copyright_created_as_limited_statutory_category, conventional).
narrative_ontology:cs_axiom('389bde7e-69fe-4946-8f30-9c07a50f7ce4', secondary, term_expiry_serves_public_learning).
narrative_ontology:cs_axiom_status(term_expiry_serves_public_learning, holdable).
narrative_ontology:cs_axiom_grounding('389bde7e-69fe-4946-8f30-9c07a50f7ce4', term_expiry_serves_public_learning, instrumental).
narrative_ontology:cs_reference_frame('389bde7e-69fe-4946-8f30-9c07a50f7ce4', limited_regulatory_tool_for_learning).
narrative_ontology:cs_drift_state('389bde7e-69fe-4946-8f30-9c07a50f7ce4', donaldson_becket_aftermath, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('389bde7e-69fe-4946-8f30-9c07a50f7ce4', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, working_authors).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, future_generations_of_readers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_perpetual_rights_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_perpetual_rights_holders).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, working_authors).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, encouragement_of_learning_doctrine).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, limited_term_exclusivity_principle).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_domain_concept).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and enacted the 1710 statute after the Licensing Act lapsed, fixing fourteen-year terms with a fourteen-year renewal for new books and twenty-one years for existing copies, requiring registration at Stationers' Hall, and providing remedies against unauthorized reprinting. Reviews petitions about the trade's operation and can amend the terms or let grants lapse as written.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, parliament_legislators, agenda_setter,
    institutional, generational, mobile, national).

% London Company members whose customary perpetual titles in old copies lost their footing when the statute fixed terms. They now hold exclusive rights in new works chiefly by buying assignments from authors with their trade capital. They lost the open-ended rent stream on backlist copies but gained registered, litigable titles and an orderly market in shares; they fund piracy prosecutions and petition Parliament for longer terms.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_perpetual_rights_holders, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_perpetual_rights_holders, beneficiary).

% Gain a fourteen-year, renewable exclusive right in what they compose, which they can sell outright to a bookseller for a lump sum or retain for renewal. Most lack printing capital and credit, so they assign early and cheaply; they bear registration formalities and the risk that a book fails to earn back its advance.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, working_authors, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__conceptual_emergence_reading, working_authors, payer).

% Buys new books during each exclusive term at prices set above the cost of reprinting, then obtains the same works without restriction once the terms lapse. Has no organized voice in drafting or amendment; its protection is the expiry written into every grant.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public, beneficiary,
    powerless, generational, constrained, national).

% Inherit every expired work free of exclusive control and pay nothing toward the bargains struck decades earlier. No seat represents this interest when terms are set; it receives whatever expiry schedule past parliaments happened to fix.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, future_generations_of_readers, beneficiary,
    powerless, civilizational, mobile, national).

% Scottish and Irish reprinters who copy new English books inside the statutory terms and sell them cheaper than London editions. Seizures, prosecutions, and cross-border jurisdictional friction are aimed at them; they would compete openly on price if the exclusive terms did not bar their trade.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, provincial_pirate_printers, excluded,
    moderate, biographical, mobile, regional).

% Hear registration and infringement disputes, decide what counts as a protected copy and when terms begin and end, and issue injunctions against reprinting. In 1774 the House of Lords resolves that copyright rests solely on the statute and its terms, declining to recognize a perpetual common-law right.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, courts_of_law, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_perpetual_rights_holders).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__conceptual_emergence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Finances knowledge production under a bounded bargain: creators and their assigns receive an exclusive window of returns long enough to recoup composition and printing costs, while every grant self-terminates so works revert to unrestricted public access; registration at Stationers' Hall standardizes title so rights can be bought, sold, and verified.
% TRANSFER_FUNCTION: Moves purchasing power from book buyers to rights-holding authors and booksellers during each limited term (prices above reproduction cost), and moved perpetual trade monopolies off the Stationers' old copies into a market of expiring, assignable statutory titles.
% ABSENT_VOICES: Price-sensitive readers and the unborn generations who inherit expired works had no seat in drafting or amendment; provincial reprinters were excluded by design; the 'encouragement of learning' interest was voiced rhetorically by factions rather than by any organized party actually representing readers.
% DISAPPEARANCE_RATIONALE: Without the statute, the category of a bounded, expiring exclusive right in texts would not have existed to occupy: the trade would have reorganized around renewed licensing or litigated common-law claims, and the conceptual space this reading identifies would never have opened.
% FOUNDING_PROBLEM: The 1695 lapse of the Licensing Act left authors without secure reward for composition and booksellers without legal protection against piracy, while the Stationers' customary perpetual titles rested on increasingly doubtful footing; Parliament sought to encourage learned men to compose and write useful books by securing property in copies for limited times.
% FOUNDING_PROBLEM_CORROBORATION: Recorded Commons and Lords debates and the contemporary pamphlet literature of 1706-1710 attest the piracy-and-reward problem from outside the eventual beneficiary seats; later historians of the book trade (Patterson, Feather, Rose) corroborate the founding diagnosis independently of the statute's own preamble. The Stationers' petitions also attest the piracy problem, though they were interested parties seeking restoration of their former position.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__conceptual_emergence_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__conceptual_emergence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).
:- end_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.34: during each term buyers pay above reproduction cost, but every grant self-terminates, bounding cumulative extraction by design. Suppression ends at 0.38: enforcement against reprinting is real and litigious, yet far narrower than the prior licensing regime, and waiting for expiry is always a lawful alternative. Theater is low (0.17): registration, term administration, and infringement actions perform their stated functions. Accessibility_collapse is 0.40: understanding the arrangement does not foreclose alternatives, because expiry restores open access on a known clock. Resistance is 0.55: the booksellers' fifty-year campaign for perpetual common-law copyright, culminating in the litigation resolved in 1774, plus persistent provincial reprinting, met the arrangement with organized opposition. The suppression_requirement series traces enforcement capacity: it builds through mid-century piracy wars, peaks around 0.46, then falls after the 1774 decision repudiates perpetual-right enforcement. All three series run on one shared nine-point grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the Company bench, the statute destroyed inherited perpetuity - a genuine loss - even while the same members collected the new regime's rents; from the reader's seat, the same arrangement prices books temporarily and then frees them forever; from the legislature's seat, it is a working policy instrument doing what its preamble says. The engine derives these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality for reading_public and future_generations_of_readers (the arrangement subsidizes them, increasingly over time as expiries accumulate) and low-to-moderate for working_authors, whose benefit is discounted by dependence on bookseller capital. The victim declaration pushes stationers_perpetual_rights_holders toward the target end, but that derivation misreads their net position: they absorbed a one-time loss of perpetuity and then became the arrangement's principal rent collectors through assignments. Because the automatic chain reads the payer role and overshoots, an explicit override sets the organized power atom to d=0.5 (symmetric) - the only stakeholder at that atom, so the correction lands solely on the booksellers. Parliament sits near the beneficiary end (institutional stake in a functioning instrument); the courts take the analytical seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical mislabels. Reading the statute as pure extraction (because identifiable losers existed and enforcement was active) ignores that the learning function was real and the expiry genuine in this window; reading it as pure coordination (because the preamble says 'learning') erases the Stationers' dispossession and the enforcement machinery. The transitional-instrument type with a declared sunset captures the design whose justification is the passage to public access, not the steady state. The obsolescence question is premature inside this interval: the instrument was young and its mandate live, so declaring the mandate outlived would misdate a decay that, if it came, arrived through later extension politics beyond this story's endpoint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the conceptual_emergence_reading of the statute_of_anne_ip_foundation kernel; what structural deltas would the sibling readings introduce?',
    'Cross-read the three linked family stories: the institutional_reallocation_reading relocates the beneficiary/victim structure onto authors and the Stationers and re-authors epsilon over a rights-transfer referent; the entangled_event_reading denies that the conceptual and institutional dimensions are separable and merges this epsilon with the institutional story''s.',
    'If the institutional reading is adopted, the victim set shifts from perpetual-monopoly holders toward dispossessed authors and the extraction profile rises; if the entangled reading is adopted, this story''s clean epsilon is invalid and the family collapses into a single story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story is one reading of a three-reading kernel; sibling deltas are recorded here, not folded into the epsilon.').

omega_variable(
    conceptual_institutional_separability,
    'Can the conceptual emergence of copyright-as-a-limited-category be assessed separately from the simultaneous institutional reallocation of rights?',
    'Counterfactual institutional analysis: ask whether the new category could have been enacted without displacing the Stationers'' holdings, and whether the displacement could have occurred without the new category.',
    'If the dimensions are inseparable, this story''s epsilon understates the arrangement''s conflict content and the transitional-instrument claim weakens toward a hybrid coordination/extraction structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_institutional_separability, conceptual, 'Whether the conceptual delta this reading isolates is genuinely separable from the institutional delta.').

omega_variable(
    learning_beneficiary_substance,
    'Is public learning a structural beneficiary of the arrangement, or a rhetorical figure deployed by trade factions seeking protection after the Licensing Act lapse?',
    'Track term expiries and price behavior after lapses: did works actually become freely accessible and cheaper, and did any drafting coalition represent reader-side interests rather than trade or author interests?',
    'If learning is rhetorical cover, the beneficiary set shrinks to authors and booksellers, the coordination-function gate weakens, and the computed type drifts toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(learning_beneficiary_substance, empirical, 'Whether the declared learning beneficiary is substantive or a preamble figure.').

omega_variable(
    sunset_integrity_under_extension_pressure,
    'Does the limited-term design hold as a genuine sunset, given the booksellers'' fifty-year campaign for perpetual common-law copyright and the later politics of term extension?',
    'Compare statutory terms against the realized duration of effective exclusivity, including rights-holder behavior after the 1774 decision and subsequent extension episodes.',
    'If extensions and renewal purchases systematically swallow expiries, the sunset is nominal and the transitional-instrument reading decays toward a hybrid or extractive structure; if expiries bite, the reading stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_integrity_under_extension_pressure, empirical, 'Whether the built-in expiry functioned as designed or was eroded by extension pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0, 64).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soanne_conceptual_tr_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(soanne_conceptual_tr_t0, observed).
narrative_ontology:measurement(soanne_conceptual_tr_t8, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement_basis(soanne_conceptual_tr_t8, observed).
narrative_ontology:measurement(soanne_conceptual_tr_t16, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement_basis(soanne_conceptual_tr_t16, observed).
narrative_ontology:measurement(soanne_conceptual_tr_t24, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement_basis(soanne_conceptual_tr_t24, observed).
narrative_ontology:measurement(soanne_conceptual_tr_t32, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement_basis(soanne_conceptual_tr_t32, observed).
narrative_ontology:measurement(soanne_conceptual_tr_t40, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement_basis(soanne_conceptual_tr_t40, observed).
narrative_ontology:measurement(soanne_conceptual_tr_t48, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 48, 0.15).
narrative_ontology:measurement_basis(soanne_conceptual_tr_t48, observed).
narrative_ontology:measurement(soanne_conceptual_tr_t56, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 56, 0.16).
narrative_ontology:measurement_basis(soanne_conceptual_tr_t56, observed).
narrative_ontology:measurement(soanne_conceptual_tr_t64, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 64, 0.17).
narrative_ontology:measurement_basis(soanne_conceptual_tr_t64, observed).

% Extraction over time
narrative_ontology:measurement(soanne_conceptual_be_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(soanne_conceptual_be_t0, observed).
narrative_ontology:measurement(soanne_conceptual_be_t8, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement_basis(soanne_conceptual_be_t8, observed).
narrative_ontology:measurement(soanne_conceptual_be_t16, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 16, 0.24).
narrative_ontology:measurement_basis(soanne_conceptual_be_t16, observed).
narrative_ontology:measurement(soanne_conceptual_be_t24, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 24, 0.25).
narrative_ontology:measurement_basis(soanne_conceptual_be_t24, observed).
narrative_ontology:measurement(soanne_conceptual_be_t32, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement_basis(soanne_conceptual_be_t32, observed).
narrative_ontology:measurement(soanne_conceptual_be_t40, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 40, 0.29).
narrative_ontology:measurement_basis(soanne_conceptual_be_t40, observed).
narrative_ontology:measurement(soanne_conceptual_be_t48, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 48, 0.31).
narrative_ontology:measurement_basis(soanne_conceptual_be_t48, observed).
narrative_ontology:measurement(soanne_conceptual_be_t56, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 56, 0.33).
narrative_ontology:measurement_basis(soanne_conceptual_be_t56, observed).
narrative_ontology:measurement(soanne_conceptual_be_t64, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 64, 0.34).
narrative_ontology:measurement_basis(soanne_conceptual_be_t64, observed).

% Suppression requirement over time
narrative_ontology:measurement(soanne_conceptual_su_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(soanne_conceptual_su_t0, observed).
narrative_ontology:measurement(soanne_conceptual_su_t8, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement_basis(soanne_conceptual_su_t8, observed).
narrative_ontology:measurement(soanne_conceptual_su_t16, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement_basis(soanne_conceptual_su_t16, observed).
narrative_ontology:measurement(soanne_conceptual_su_t24, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 24, 0.39).
narrative_ontology:measurement_basis(soanne_conceptual_su_t24, observed).
narrative_ontology:measurement(soanne_conceptual_su_t32, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 32, 0.42).
narrative_ontology:measurement_basis(soanne_conceptual_su_t32, observed).
narrative_ontology:measurement(soanne_conceptual_su_t40, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement_basis(soanne_conceptual_su_t40, observed).
narrative_ontology:measurement(soanne_conceptual_su_t48, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 48, 0.46).
narrative_ontology:measurement_basis(soanne_conceptual_su_t48, observed).
narrative_ontology:measurement(soanne_conceptual_su_t56, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 56, 0.44).
narrative_ontology:measurement_basis(soanne_conceptual_su_t56, observed).
narrative_ontology:measurement(soanne_conceptual_su_t64, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 64, 0.38).
narrative_ontology:measurement_basis(soanne_conceptual_su_t64, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__conceptual_emergence_reading, resource_allocation).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, institutional_reallocation_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, entangled_event_reading).

% DUAL FORMULATION NOTE:
% The statute_of_anne_ip_foundation kernel decomposes into three epsilon-invariant stories: this conceptual-emergence reading (a new category is created; epsilon over the limited-instrument arrangement), the institutional_reallocation_reading (occupants change in an existing space; epsilon over the rights-transfer arrangement), and the entangled_event_reading (anti-decomposition; asserts no separable epsilon exists). This file links both siblings per the family rule. Relation structure: this reading's core premise (the conceptual delta is isolable with its own stable epsilon) directly contradicts the entangled reading's core premise (the dimensions cannot be disentangled), so the edge to that sibling is foreclosure; the institutional reading is a rival emphasis compatible with hybrid positions, so the edge is coexistence. Upstream/downstream: the conceptual reading supplies the category vocabulary the reallocation reading presupposes, while the entangled reading contests both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statute_of_anne_ip_foundation__conceptual_emergence_reading, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
