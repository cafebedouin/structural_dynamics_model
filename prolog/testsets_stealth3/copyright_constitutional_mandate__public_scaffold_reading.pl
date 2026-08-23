% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__public_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__public_scaffold_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__public_scaffold_reading
 *   human_readable: Copyright as Public-Domain Scaffold (Public Scaffold Reading)
 *   domain: legal/economic/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the copyright constitutional
 *   mandate kernel: the public_scaffold_reading, under which exclusive rights
 *   are a deliberately transitional device — granted to induce creation,
 *   expiring by design so that works enrich the public domain. The
 *   beneficiary is the diffuse, successive public; no victim class is
 *   declared because the interim restraint is the designed price of the
 *   incentive, not extraction. The ε referent is the standing arrangement
 *   (copyright law as it operates, with its current terms and enforcement
 *   machinery) assessed by this reading's own lights: the reading registers
 *   real drift — terms extended far past the founding calibration,
 *   enforcement machinery grown — without abandoning the claim that the
 *   arrangement remains a transitional device whose sunset still functions.
 *   The claim/metric gap is deliberate and load-bearing: claimed_type is
 *   scaffold because the arrangement's defining feature is its designed
 *   terminability and continuous public-domain inflow; the metrics describe
 *   the drift this reading itself diagnoses. Sibling readings
 *   (corporate_enclosure_reading, judicial_ambiguity_reading) are separate
 *   constraints in separate files, linked via network.affects_constraints.
 *   KEY AGENTS (by structural relationship): - us_congress: Agenda setter
 *   ([institutional]/[constrained]) — calibrates terms and exceptions inside
 *   the constitutional frame - federal_courts: Adjudicating arm of the agenda
 *   ([institutional]/[constrained]) — interprets fair use, reviews term
 *   extensions - working_authors: Grant recipients ([moderate]/[mobile]) —
 *   hold the temporary exclusive right; may opt out entirely -
 *   large_publishers_studios_labels: Catalog administrators
 *   ([institutional]/[arbitrage]) — hold most exclusive rights; steer term
 *   policy - readers_scholars_and_educators: Access-side beneficiaries
 *   ([organized]/[constrained]) — consume under license, fair use, and expiry
 *   - libraries_archives_and_museums: Preservation stewards
 *   ([organized]/[constrained]) — manage the transition into common
 *   availability - public_domain_building_creators: Downstream builders
 *   ([moderate]/[constrained]) — their inputs arrive only as terms lapse -
 *   orphan_works_users: Unrepresented users ([powerless]/[trapped]) — blocked
 *   by unlocatable ownership, no seat in calibration debates -
 *   copyright_reform_scholars: Analytical observers
 *   ([analytical]/[analytical]) — measure the arrangement against its stated
 *   end
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__public_scaffold_reading, 0.46).
domain_priors:suppression_score(copyright_constitutional_mandate__public_scaffold_reading, 0.48).
domain_priors:theater_ratio(copyright_constitutional_mandate__public_scaffold_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__public_scaffold_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__public_scaffold_reading, "Copyright as Public-Domain Scaffold (Public Scaffold Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__public_scaffold_reading, "legal/economic/political").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:has_sunset_clause(copyright_constitutional_mandate__public_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__public_scaffold_reading, '23a32533-c0a1-492b-b427-e8ee93e464e5').
narrative_ontology:cs_kernel_codification('23a32533-c0a1-492b-b427-e8ee93e464e5', fixed_text).
narrative_ontology:cs_authority_grounding('23a32533-c0a1-492b-b427-e8ee93e464e5', lineage).
narrative_ontology:cs_interpretation_layer_present('23a32533-c0a1-492b-b427-e8ee93e464e5').
narrative_ontology:cs_reading_relation('23a32533-c0a1-492b-b427-e8ee93e464e5', copyright_constitutional_mandate__corporate_enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('23a32533-c0a1-492b-b427-e8ee93e464e5', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('23a32533-c0a1-492b-b427-e8ee93e464e5', foundational, public_domain_as_constitutive_endpoint).
narrative_ontology:cs_axiom_status(public_domain_as_constitutive_endpoint, holdable).
narrative_ontology:cs_axiom_grounding('23a32533-c0a1-492b-b427-e8ee93e464e5', public_domain_as_constitutive_endpoint, instrumental).
narrative_ontology:cs_axiom('23a32533-c0a1-492b-b427-e8ee93e464e5', secondary, limited_times_non_perpetuity_rule).
narrative_ontology:cs_axiom_status(limited_times_non_perpetuity_rule, holdable).
narrative_ontology:cs_axiom_grounding('23a32533-c0a1-492b-b427-e8ee93e464e5', limited_times_non_perpetuity_rule, conventional).
narrative_ontology:cs_reference_frame('23a32533-c0a1-492b-b427-e8ee93e464e5', limited_times_public_good_bargain).
narrative_ontology:cs_drift_state('23a32533-c0a1-492b-b427-e8ee93e464e5', post_ctea_digital_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('23a32533-c0a1-492b-b427-e8ee93e464e5', '2026-06-11T12:00:00Z').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, working_authors).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, public_domain_building_creators).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, readers_scholars_and_educators).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, libraries_archives_and_museums).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, large_publishers_studios_labels).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, quid_pro_quo_bargain_theory).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, idea_expression_dichotomy).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, limited_times_non_perpetuity_rule).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and amends the copyright statutes: sets term lengths, defines the exclusive rights and their exceptions, and ratifies treaty obligations that bind future calibration. Operates inside a constitutional frame permitting exclusive rights only for limited times to promote progress. Receives sustained attention and campaign support from content-industry constituents, while the beneficiaries of term expiry are diffuse, unidentified in advance, and unorganized — so the access side of the ledger rarely appears as a constituency at calibration time.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, us_congress, agenda_setter,
    institutional, biographical, constrained, national).

% Adjudicate infringement disputes, draw the boundaries of fair use, and review term extensions for constitutional validity. Bound by statute and precedent; in the leading term-extension case the highest court upheld congressional calibration under deferential review, absorbing the accumulated drift into doctrine rather than forcing a textual reckoning.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, federal_courts, agenda_setter,
    institutional, generational, constrained, national).

% Receive the temporary exclusive right automatically upon fixing a work — the incentive payment the system is built around. Most earn little from it directly; a small fraction command significant licensing income. Any author may decline the exclusive right entirely by dedicating a work to the public domain or licensing it openly, so participation is formally voluntary and exit is a real, exercised option.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, working_authors, beneficiary,
    moderate, biographical, mobile, global).

% Hold and administer the large majority of exclusive rights in commercial catalogs across books, recordings, film, and software. Finance much commissioned creation, control distribution channels, and run the most sustained advocacy on term length and enforcement scope. Term income concentrates here rather than with individual authors; portfolios can be shifted across territories, formats, and licensing models as conditions change.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, large_publishers_studios_labels, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__public_scaffold_reading, large_publishers_studios_labels, agenda_setter).

% Consume and teach from works under purchase, license, fair use, and library exceptions. Benefit twice over: from new works the incentive induces, and from the continuous inflow of expired works into unrestricted availability. Have no seat in term-setting beyond general civic channels, and depend on judicially maintained exceptions for much of their lawful use.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, readers_scholars_and_educators, beneficiary,
    organized, immediate, constrained, global).

% Preserve collections, lend and display works, and manage the practical transition of materials into common availability as terms lapse. Operate under specific statutory exceptions; mass digitization routinely waits on expiry because clearing rights for orphaned and out-of-print material costs more than program budgets allow.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, libraries_archives_and_museums, beneficiary,
    organized, generational, constrained, national).

% Make work that depends on access to earlier material — documentarians cutting archival footage, dramatists adapting novels, musicians sampling recordings, translators, annotators. Their raw material expands each year as terms expire; every extension postpones their inputs. Unlike authors, they cannot opt out of other people's terms — their only lever is waiting.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, public_domain_building_creators, beneficiary,
    moderate, biographical, constrained, global).

% Want to republish or adapt works whose owners cannot be located — out-of-print books, abandoned photographs, output of defunct producers. Clearance is impossible and liability is statutory, so projects stall indefinitely. They have no organized representation in term and exception debates and no procedural path to regularize their position.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, orphan_works_users, excluded,
    powerless, immediate, trapped, national).

% Legal academics and economists who measure whether term lengths, exceptions, and enforcement still track the arrangement's stated public end. Produce the optimal-duration literature, appear as amici in constitutional challenges, and supply the analytical record the other seats argue over. Hold no stake in term income and no vote in calibration.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, copyright_reform_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__public_scaffold_reading, large_publishers_studios_labels).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__public_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the public-goods problem of creative production: intangible works are non-rival and cheap to copy, so absent some return channel, underinvestment in creation follows. The arrangement gives creators a temporary exclusive return channel and guarantees the works' eventual unconditional availability, converting a private-incentive problem into a scheduled commons inflow.
% TRANSFER_FUNCTION: Moves exclusive-control income (sales premiums, licensing fees) from users and readers to authors and catalog holders during each term; then moves the completed works themselves, irrevocably and without compensation, into unrestricted public availability at term's end.
% ABSENT_VOICES: Orphan-works users have no seat and no organization; future creators not yet born — the heaviest cumulative beneficiaries of expiry — are definitionally absent; and the public domain itself has no institutional representative, because its beneficiaries are diffuse and unidentified until after works expire. Unanimity at calibration time therefore reflects who was in the room, not consent of the affected.
% DISAPPEARANCE_RATIONALE: Overnight removal would force immediate renegotiation of how creation is financed: publishing advances, licensing markets, streaming catalogs, and platform content deals all presuppose the exclusive-right framework. Patronage platforms, public funding, and open-collaboration models would expand to fill the gap, and this reading predicts eventual reconvergence on some incentive mechanism — but the rearrangement itself would be sweeping and disruptive, which is what world_rearranges records.
% FOUNDING_PROBLEM: How to finance creative production without permanent private control of culture: the Statute of Anne broke the printers' guild perpetual monopolies by granting authors time-boxed rights expressly so that learning would flourish and works would ultimately circulate freely; the constitutional framers wrote the same bargain into the Progress Clause.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Statute of Anne's own preamble states the encouragement-of-learning purpose; Madison's Federalist No. 43 frames the clause as public good coinciding with individual claim; and the public-goods economics of information (from Arrow onward) independently establishes the financing problem the arrangement answers. Historians of the book trade attest the guild-monopoly origin. No one attests from the public domain's own seat — it has none — which is itself recorded signal, not silence in the record's favor.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__public_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__public_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__public_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__public_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__public_scaffold_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).
:- end_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.46 (low-to-moderate): the term-limited exclusive right imposes real interim costs on access and reuse, but those costs are bounded by expiry, filtered by fair use and library exceptions, and exchanged for a continuous inflow of works into common availability. Suppression is 0.48: enforcement is real (statutory damages, takedown and anticircumvention machinery) but aims at protecting the bounded exclusivity, not at sealing exits from the arrangement itself — any author may walk away by dedicating a work or licensing openly. Note the asymmetry the engine owns: suppression is a raw structural property left unscaled, while extractiveness is scaled by directionality and spatial scope in the engine's computation. Theater_ratio is 0.38 and rising: retroactive term extensions confer zero prospective incentive on already-created works, so a growing share of the arrangement's justification rhetoric is performative maintenance of terms whose incentive function has lapsed. Accessibility_collapse is 0.30 — alternatives (public domain, fair use, open licensing, independent creation) remain abundantly available. Resistance is 0.55 — organized, sustained criticism (constitutional challenge to the 1998 extension, commons and open-access movements, orphan-works advocacy) meets the arrangement at every calibration episode. Receipt surface: term income demonstrably concentrates in catalog-owning intermediaries, so gain_flow names large_publishers_studios_labels even though the designed beneficiary is the diffuse public — receipt is not benefit. Fixing_cost is prohibitive: treaty minimums (Berne, TRIPS) bind signatories against unilateral shortening, reliance interests attach to existing catalogs, and the diffuse beneficiary side has no organized hand to play at the veto points. The measurement series run on one shared time grid (years since 1787) so every tracked metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat (Congress, and the publishers who most reliably appear before it) the arrangement is a functioning incentive system it administers; from the access-side seats (readers, libraries, downstream builders) the same structure is a lengthening wait punctuated by exceptions; from the analytical seat it is a measurable divergence between stated end and operative calibration. Same-level lateral divergence is sharpest between working_authors and public_domain_building_creators — both moderate-power actors, but the author's exit (opt out of the exclusive right entirely) is real while the builder's is not: builders cannot opt out of other people's terms, only wait for them. The engine computes per-seat classifications from the structural data; this story does not adjudicate between the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared beneficiaries are diffuse or successive — working authors (grant recipients), builders and readers (expiry-side), libraries (transition stewards) — so the derivation places them near the beneficiary end of directionality, with effective extraction damped or inverted for their seats. No victims are declared: under this reading no party is extracted-from; the interim restraint is the purchased input to the public-domain end. The nearest-to-target seat is orphan_works_users (trapped, unrepresented), but their blockage is a coverage gap in the arrangement rather than its product, and they carry the excluded role rather than payer. Congress sits above pure symmetry: an agenda setter with no direct claim on term income but sustained indirect benefit from content-industry support — the derivation should land it moderately above the beneficiary pole, and no override is authored because the qualitative placement, not a precise value, is what the structural data supports. Publishers collect the largest share of term income yet do so as instruments of the grant; their d stays low despite their agenda-setting weight. Spatial scope is national statute coupled through international treaties — the engine's scope amplification applies modestly.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification is what keeps both misreadings visible. Read as pure coordination, the drift documented in the measurement series (terms lengthening, enforcement hardening, theater rising) would be invisible — nothing in a rope's profile flags a sunset quietly being pushed outward. Read as pure extraction, the arrangement's genuine ongoing function would be denied: works enter the public domain every year, fair use resolves hard cases, and the founding bargain still finances creation. The R5 interview comes back coherent: founding problem live (financing creative production without permanent enclosure is an unsolved standing problem), disappearance verdict world_rearranges (publishing advances, licensing markets, and platform content economies depend on the framework) — live-status paired with rearrange-verdict is the healthy signature, not the dead-status/zombie mismatch. The number to watch is theater_ratio: it crossed a third of activity by the 1998 extension and trends upward; if the performative share overtakes the functional share, this reading's own lights would demand recalibration, and the corpus should expect a drift signature toward the enclosure sibling rather than a quiet continuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the public_scaffold_reading of the copyright_constitutional_mandate kernel; would instantiating corporate_enclosure_reading or judicial_ambiguity_reading instead change the beneficiary structure and effective extraction enough to change the classification?',
    'Read the three sibling stories together: compare beneficiary concentration, victim presence, sunset vitality, and computed per-seat types across the family; the kernel''s classification is the set of readings, not any single member.',
    'Under the enclosure reading the same statute computes with concentrated rightsholder beneficiaries and a resisted sunset (materially higher effective extraction); under the deference reading classification relocates to legislative discretion and the analytical seat loses standing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which reading of the copyright kernel is instantiated determines the constraint''s structural identity.').

omega_variable(
    marginal_incentive_term_threshold,
    'At what term length does the marginal incentive effect on new creation approach zero while the cost of withheld works keeps accruing?',
    'Econometric optimal-term estimation (discounted welfare models of copyright duration) combined with longitudinal data on the commercial lifespan of works in different media.',
    'If current terms greatly exceed the incentive-optimal span, the late-interval extractiveness in this series is understated and drift toward enclosure accelerates; if not, the low-to-moderate profile this reading authors stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_incentive_term_threshold, empirical, 'Whether current terms outrun the incentive function used to justify them.').

omega_variable(
    public_domain_representation_deficit,
    'Does the absence of any organized constituency speaking for the public domain systematically bias term-setting toward rightsholder interests?',
    'Institutional analysis of hearing participation and lobbying records across the major term-extension episodes (1909, 1976, 1998), weighed against observable diffuse-beneficiary turnout.',
    'If confirmed, the arrangement''s coordination function degrades faster than the metric series shows and the enclosure sibling becomes the better description; if refuted, diffuse beneficiaries are adequately proxied by existing civic channels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_domain_representation_deficit, empirical, 'Whether unrepresented diffuse beneficiaries bias the arrangement''s evolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__public_scaffold_reading, 0, 238).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copyright_scaffold_tr_t0, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(copyright_scaffold_tr_t0, observed).
narrative_ontology:measurement(copyright_scaffold_tr_t44, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 44, 0.12).
narrative_ontology:measurement_basis(copyright_scaffold_tr_t44, observed).
narrative_ontology:measurement(copyright_scaffold_tr_t122, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 122, 0.18).
narrative_ontology:measurement_basis(copyright_scaffold_tr_t122, observed).
narrative_ontology:measurement(copyright_scaffold_tr_t189, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 189, 0.26).
narrative_ontology:measurement_basis(copyright_scaffold_tr_t189, observed).
narrative_ontology:measurement(copyright_scaffold_tr_t211, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 211, 0.34).
narrative_ontology:measurement_basis(copyright_scaffold_tr_t211, observed).
narrative_ontology:measurement(copyright_scaffold_tr_t238, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 238, 0.38).
narrative_ontology:measurement_basis(copyright_scaffold_tr_t238, observed).

% Extraction over time
narrative_ontology:measurement(copyright_scaffold_be_t0, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(copyright_scaffold_be_t0, observed).
narrative_ontology:measurement(copyright_scaffold_be_t44, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 44, 0.24).
narrative_ontology:measurement_basis(copyright_scaffold_be_t44, observed).
narrative_ontology:measurement(copyright_scaffold_be_t122, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 122, 0.3).
narrative_ontology:measurement_basis(copyright_scaffold_be_t122, observed).
narrative_ontology:measurement(copyright_scaffold_be_t189, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 189, 0.38).
narrative_ontology:measurement_basis(copyright_scaffold_be_t189, observed).
narrative_ontology:measurement(copyright_scaffold_be_t211, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 211, 0.44).
narrative_ontology:measurement_basis(copyright_scaffold_be_t211, observed).
narrative_ontology:measurement(copyright_scaffold_be_t238, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 238, 0.46).
narrative_ontology:measurement_basis(copyright_scaffold_be_t238, observed).

% Suppression requirement over time
narrative_ontology:measurement(copyright_scaffold_su_t0, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(copyright_scaffold_su_t0, observed).
narrative_ontology:measurement(copyright_scaffold_su_t44, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 44, 0.18).
narrative_ontology:measurement_basis(copyright_scaffold_su_t44, observed).
narrative_ontology:measurement(copyright_scaffold_su_t122, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 122, 0.24).
narrative_ontology:measurement_basis(copyright_scaffold_su_t122, observed).
narrative_ontology:measurement(copyright_scaffold_su_t189, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 189, 0.33).
narrative_ontology:measurement_basis(copyright_scaffold_su_t189, observed).
narrative_ontology:measurement(copyright_scaffold_su_t211, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 211, 0.45).
narrative_ontology:measurement_basis(copyright_scaffold_su_t211, observed).
narrative_ontology:measurement(copyright_scaffold_su_t238, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 238, 0.48).
narrative_ontology:measurement_basis(copyright_scaffold_su_t238, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__public_scaffold_reading, resource_allocation).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, judicial_ambiguity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'copyright' decomposes into three structurally distinct constraints sharing one constitutional text (epsilon-invariance decomposition of the copyright_constitutional_mandate kernel). This scaffold reading carries low-to-moderate epsilon with diffuse beneficiaries and a functioning sunset; corporate_enclosure_reading carries concentrated rightsholder beneficiaries, a resisted sunset, and materially higher epsilon; judicial_ambiguity_reading relocates classification authority to legislative discretion and drops the analytical seat's standing. Family linkage runs through affects_constraints in all three files; this reading is upstream in the evaluative sense — it supplies the public-good standard against which the other two readings' term policies must defend themselves.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
