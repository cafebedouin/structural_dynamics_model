% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__republican_reading, []).

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
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Republican Reading of Sovereign Legitimacy — Delegated Popular Consent
 *   domain: political philosophy/constitutional theory
 *
 * SUMMARY:
 *   In the republican reading of political legitimacy, authority is
 *   legitimate only when it flows upward from the people subject to it:
 *   officeholders hold power as delegates on bounded, revocable trust, and
 *   each electoral cycle re-validates the delegation. The arrangement solves
 *   a real coordination problem — converting the question of who may wield
 *   state coercion from a matter of succession war and inheritance into a
 *   scheduled, repeatable procedure — while carrying a persistent asymmetry:
 *   everyone inside the franchise boundary helps authorize the system, and
 *   everyone outside it (resident non-citizens, children, disenfranchised
 *   convicted persons, and, across most of the interval, women and the
 *   propertyless) is governed by outputs they had no hand in authorizing,
 *   with concentrated minorities additionally exposed to repeated majority
 *   override. This file instantiates one reading of the sovereign_legitimacy
 *   kernel as a clean, single-epsilon constraint; the monarchical and
 *   constitutional-hybrid readings are separate stories in the same family,
 *   linked through network.affects_constraints. Assumption: the interval maps
 *   t=0 to roughly 1870 and t=150 to roughly 2020, tracing a consolidated
 *   republic's franchise expansion and enforcement history.
 *
 * KEY AGENTS:
 *   - enfranchised_electorate: Primary beneficiary (organized/constrained) — authorizes and dismisses governments through the vote
 *   - elected_officeholders: Secondary beneficiary and administrator (institutional/mobile) — hold delegated authority and renew it through the electoral machinery they run
 *   - constitutional_courts: Enforcement seat (institutional/constrained) — police the boundary of delegated power against the constitutional text
 *   - disenfranchised_residents: Primary target (powerless/trapped) — governed under full coercive incidence with no consent mechanism
 *   - outvoted_minority_factions: Recurring target (organized/constrained) — full participatory standing, repeated substantive defeat
 *   - future_generations: Temporal target (powerless/trapped) — bear long-horizon costs no ballot reaches
 *   - comparative_constitutional_scholars: Analytical observer — sees the full structure across regimes and eras
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.45).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.4).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Republican Reading of Sovereign Legitimacy — Delegated Popular Consent").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political philosophy/constitutional theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, 'd641c625-2c23-4d7d-adc7-4087714e582c').
narrative_ontology:cs_kernel_codification('d641c625-2c23-4d7d-adc7-4087714e582c', formalized).
narrative_ontology:cs_authority_grounding('d641c625-2c23-4d7d-adc7-4087714e582c', practice).
narrative_ontology:cs_interpretation_layer_present('d641c625-2c23-4d7d-adc7-4087714e582c').
narrative_ontology:cs_reading_relation('d641c625-2c23-4d7d-adc7-4087714e582c', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('d641c625-2c23-4d7d-adc7-4087714e582c', sovereign_legitimacy__constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('d641c625-2c23-4d7d-adc7-4087714e582c', foundational, legitimate_authority_requires_governed_consent).
narrative_ontology:cs_axiom_status(legitimate_authority_requires_governed_consent, holdable).
narrative_ontology:cs_axiom_grounding('d641c625-2c23-4d7d-adc7-4087714e582c', legitimate_authority_requires_governed_consent, deontological).
narrative_ontology:cs_axiom('d641c625-2c23-4d7d-adc7-4087714e582c', secondary, delegated_power_is_revocable_trust).
narrative_ontology:cs_axiom_status(delegated_power_is_revocable_trust, holdable).
narrative_ontology:cs_axiom_grounding('d641c625-2c23-4d7d-adc7-4087714e582c', delegated_power_is_revocable_trust, instrumental).
narrative_ontology:cs_reference_frame('d641c625-2c23-4d7d-adc7-4087714e582c', delegated_popular_consent).
narrative_ontology:cs_drift_state('d641c625-2c23-4d7d-adc7-4087714e582c', contemporary_mass_electorate, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d641c625-2c23-4d7d-adc7-4087714e582c', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, enfranchised_electorate).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, elected_officeholders).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, disenfranchised_residents).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, outvoted_minority_factions).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, future_generations).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, social_contract_theory).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, consent_of_the_governed_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adult citizens holding the vote select officeholders at scheduled intervals, petition, run for office, and ratify or reject policies through referenda. The arrangement gives them the standing to authorize and dismiss governments. Leaving it means emigration and loss of membership in the polity, which few undertake.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, enfranchised_electorate, beneficiary,
    organized, biographical, constrained, national).

% Compete for and hold delegated authority for fixed terms: they set legislative agendas, command the executive, and administer the election machinery that renews their own mandates. When terms end or voters remove them they return to private life; many rotate between office, party posts, and lobbying.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, elected_officeholders, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, elected_officeholders, agenda_setter).

% Review legislation and executive action against the constitutional text that records the terms of delegation; strike down measures that exceed delegated powers or violate enumerated rights. Their rulings bind every other seat, and their own composition is shaped by the officeholders they check.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Live under the full coercive force of laws, taxes, policing, and administrative rule while holding no vote and no candidacy path: resident non-citizens, children, people with felony convictions in disenfranchising jurisdictions, and historically women and the propertyless. The consent mechanisms are closed to them, and exit requires crossing borders that other states police.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, disenfranchised_residents, payer,
    powerless, biographical, trapped, national).

% Organized groups — religious, ethnic, ideological, economic — that participate fully in elections yet regularly lose majority votes on matters central to their way of life. Their recourse is litigation, internal migration to friendlier jurisdictions, or persuasion at the next cycle; their strongest preferences are repeatedly overridden.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, outvoted_minority_factions, payer,
    organized, generational, constrained, regional).

% People not yet of age or not yet born who will inherit the long-horizon consequences of present majoritarian decisions — public debt, environmental degradation, entrenched constitutional settlements. No electoral mechanism reaches them; their interests enter only through the foresight or advocacy of current voters.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, future_generations, payer,
    powerless, civilizational, trapped, global).

% Study legitimacy arrangements across regimes and eras: they document how franchise boundaries expand or contract, compare consent-based settlements with inherited-authority systems, and publish assessments that feed reform debates and constitutional design elsewhere.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__republican_reading, elected_officeholders).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts the problem of who may wield state coercion — otherwise settled by succession war, coup, or inheritance — into a scheduled, repeatable procedure: competitive elections select and remove officeholders, fixed terms bound their tenure, and constitutional rules record the terms of the delegation.
% TRANSFER_FUNCTION: Moves decision authority over taxation, law, and force from the resident population at large to elected representatives and executives for bounded terms; moves accountability pressure back to the electorate at each cycle; and moves the costs of majority preferences onto residents whom the franchise excludes.
% ABSENT_VOICES: The disenfranchised — resident non-citizens, children, convicted persons in disenfranchising jurisdictions, and historically women and the propertyless — together with future generations. They bear the outputs of the machine but were never seated at its input; they stand outside the very franchise boundary the arrangement itself draws.
% DISAPPEARANCE_RATIONALE: If the upward-flow legitimacy arrangement vanished overnight, every sitting government would lose its warrant simultaneously: succession would revert to force, inheritance, or divine claim, and the entire apparatus of scheduled transfer, removal, and accountability — built on the delegation premise — would have nothing to rest on.
% FOUNDING_PROBLEM: How to make political obedience legitimate among moral equals once inherited and divine titles to rule lost their self-evidence — and, practically, how to stop the recurrent civil wars fought over succession.
% FOUNDING_PROBLEM_CORROBORATION: Contract-era theorists framed the problem before any modern republic benefited from the answer; the historical record of succession conflicts corroborates the practical stakes; suffragist and abolitionist dissenters inside republics attested the gap between claimed and actual consent from outside the benefiting set; contemporary comparative-politics scholarship and international election-monitoring bodies attest the problem remains live. Attestation does not come only from the arrangement's beneficiaries.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__republican_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__republican_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).
:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are independent authored facts. The claim — tangled_rope — rests on structure alone: a genuine coordination function (peaceful selection and removal of rulers), a named paying side (the franchise-excluded and the repeatedly-outvoted), and active enforcement (election administration, constitutional adjudication, suppression of extra-electoral seizure). The metrics describe observed operation. Base extractiveness is authored at 0.45 — moderate, per this reading's own lights: the delegation itself is the endorsed core, the exclusions are the acknowledged cost — and the temporal series shows it falling from 0.62 to 0.44 as the franchise broadened, with a slight late uptick as felony disenfranchisement, growing non-citizen resident populations, and intensified majoritarian override offset universal-suffrage gains. Suppression (0.40) is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled in the engine's computation. Theater (0.28) tracks the growth of plebiscitary ceremony and money-mediated competition atop a still-functional electoral core. All three series share one six-point grid (t = 0, 30, 60, 90, 120, 150) so no metric row borrows another's endpoints. Resistance (0.5) reflects real, partly successful challenge — suffrage and civil-rights movements literally moved the franchise boundary, which is why the extractiveness series declines. Accessibility collapse (0.6) is partial: inside this framework inherited-right claims lose their warrant, yet hybrid arrangements persist externally.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the enfranchised electorate's position the arrangement is empowerment: the mechanism that makes government dismissible. From the disenfranchised resident's position the same machinery is subjection without voice — full coercive incidence, zero authorization. Outvoted minority factions hold an ambivalent middle: real participatory standing, repeated substantive defeat. Elected officeholders experience the arrangement as the source of their authority and the schedule of their vulnerability. Constitutional courts see procedural integrity to defend. The engine computes these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (enfranchised_electorate, elected_officeholders) drive those seats toward the beneficiary end of d; victim declarations (disenfranchised_residents, outvoted_minority_factions, future_generations) drive them toward the target end. Exit modulation sharpens the spread: the disenfranchised are trapped — jurisdiction follows them — pushing them nearest full-target; the electorate is constrained, since exit means emigration and lost membership; officeholders are mobile across office, party, and lobbying roles, damping their effective extraction despite collecting the mechanism's product; minority factions' organization moderates their d relative to the isolated disenfranchised. Future generations are maximally target-positioned: no exit, no voice, full incidence. Constitutional courts carry no beneficiary or victim declaration, so their seat takes the power-atom fallback — appropriate for an enforcement seat that neither collects nor pays. No directionality overrides were needed: the declarations plus exit options already produce the intended spread.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making coercion legitimate among equals and ending recurrent succession violence — is live: every election re-performs it, and governments demonstrably change hands through the mechanism, so the function has not atrophied. Theater is rising (0.12 to 0.28) but from a low base and atop working machinery. The mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds them aligned — no zombie flag. This is not a degraded remnant: fixing its known defects (franchise gaps, majoritarian override) is costly but has historically been done, and the core function would be genuinely missed if it vanished. Mandatrophy is not resolved and is not declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This constraint is one reading of the sovereign_legitimacy kernel — how would instantiating the monarchical_reading or constitutional_hybrid_reading instead change the structural data?',
    'Author separate stories per reading and compare epsilon, beneficiary/victim sets, and computed types across the family.',
    'Under the monarchical reading the beneficiary/victim structure inverts (dynasty and court collect; subjects pay without consent mechanisms) and epsilon rises; under the hybrid reading the burden splits between ceremonial and delegated components. Classifications computed here apply only to the republican instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer structure: this file is one of three readings of the sovereign_legitimacy kernel.').

omega_variable(
    franchise_boundary_placement,
    'Who counts as ''the people'' whose consent grounds authority — where exactly does the franchise boundary sit, and who falls outside it?',
    'Comparative franchise history plus jurisdictional data on excluded resident populations (non-citizens, minors, convicted persons, historically women and the propertyless).',
    'Epsilon scales roughly with the excluded share of the governed population: a narrower boundary pushes the arrangement toward pure extraction for the excluded; a universal boundary shrinks the victim set toward the residual (minors, non-citizens, future generations).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(franchise_boundary_placement, empirical, 'Franchise boundary placement drives the extraction profile.').

omega_variable(
    majoritarian_override_design_contingency,
    'Is the burden on outvoted minorities an unavoidable structural cost of consent-based authority, or is it reducible by counter-majoritarian design (rights charters, judicial review, supermajority rules) without abandoning the consent premise?',
    'Cross-regime comparison of minority-preference survival rates under different counter-majoritarian institutions holding franchise breadth constant.',
    'If design-contingent, the extraction component is a tunable parameter and the arrangement trends toward pure coordination as safeguards mature; if structural, the hybrid coordination/extraction profile is permanent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(majoritarian_override_design_contingency, conceptual, 'Whether majoritarian tyranny is intrinsic to the arrangement or contingent on institutional design.').

omega_variable(
    consent_actual_vs_hypothetical,
    'Does the delegation claim rest on actual, meaningful consent (informed votes, real alternatives, competitive elections) or on hypothetical or tacit consent that most residents never expressly give?',
    'Political-participation research: turnout, choice competitiveness, information levels, and the condition of governed residents who never consented at all.',
    'If actual consent is required, most governed residents have never meaningfully consented and epsilon rises sharply; if hypothetical consent suffices, the moderate profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_actual_vs_hypothetical, conceptual, 'The standard of consent underlying the delegation claim.').

omega_variable(
    electoral_validation_functionality,
    'Do electoral cycles actually validate legitimacy by producing responsive representation, or do they increasingly ritualize it (money-mediated competition, low information, insulated districts)?',
    'Responsiveness studies correlating policy outputs with median-voter preferences across decades, plus campaign-finance and districting data.',
    'Confirmed ritualization raises theater_ratio and pushes the arrangement toward inertial maintenance of form without function; confirmed responsiveness supports the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_validation_functionality, empirical, 'Whether electoral validation is functional or increasingly theatrical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__republican_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(sove_tr_t0, observed).
narrative_ontology:measurement(sove_tr_t30, sovereign_legitimacy__republican_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(sove_tr_t30, observed).
narrative_ontology:measurement(sove_tr_t60, sovereign_legitimacy__republican_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement_basis(sove_tr_t60, observed).
narrative_ontology:measurement(sove_tr_t90, sovereign_legitimacy__republican_reading, theater_ratio, 90, 0.21).
narrative_ontology:measurement_basis(sove_tr_t90, observed).
narrative_ontology:measurement(sove_tr_t120, sovereign_legitimacy__republican_reading, theater_ratio, 120, 0.25).
narrative_ontology:measurement_basis(sove_tr_t120, observed).
narrative_ontology:measurement(sove_tr_t150, sovereign_legitimacy__republican_reading, theater_ratio, 150, 0.28).
narrative_ontology:measurement_basis(sove_tr_t150, observed).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__republican_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(sove_be_t0, observed).
narrative_ontology:measurement(sove_be_t30, sovereign_legitimacy__republican_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement_basis(sove_be_t30, observed).
narrative_ontology:measurement(sove_be_t60, sovereign_legitimacy__republican_reading, base_extractiveness, 60, 0.5).
narrative_ontology:measurement_basis(sove_be_t60, observed).
narrative_ontology:measurement(sove_be_t90, sovereign_legitimacy__republican_reading, base_extractiveness, 90, 0.46).
narrative_ontology:measurement_basis(sove_be_t90, observed).
narrative_ontology:measurement(sove_be_t120, sovereign_legitimacy__republican_reading, base_extractiveness, 120, 0.44).
narrative_ontology:measurement_basis(sove_be_t120, observed).
narrative_ontology:measurement(sove_be_t150, sovereign_legitimacy__republican_reading, base_extractiveness, 150, 0.45).
narrative_ontology:measurement_basis(sove_be_t150, observed).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__republican_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(sove_su_t0, observed).
narrative_ontology:measurement(sove_su_t30, sovereign_legitimacy__republican_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(sove_su_t30, observed).
narrative_ontology:measurement(sove_su_t60, sovereign_legitimacy__republican_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement_basis(sove_su_t60, observed).
narrative_ontology:measurement(sove_su_t90, sovereign_legitimacy__republican_reading, suppression_requirement, 90, 0.44).
narrative_ontology:measurement_basis(sove_su_t90, observed).
narrative_ontology:measurement(sove_su_t120, sovereign_legitimacy__republican_reading, suppression_requirement, 120, 0.41).
narrative_ontology:measurement_basis(sove_su_t120, observed).
narrative_ontology:measurement(sove_su_t150, sovereign_legitimacy__republican_reading, suppression_requirement, 150, 0.4).
narrative_ontology:measurement_basis(sove_su_t150, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__republican_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% 'Sovereign legitimacy' as a colloquial label covers three structurally distinct claims with different epsilon values, beneficiary/victim sets, and failure modes. Decomposed per the epsilon-invariance principle into a three-story family: monarchical_reading (downward flow), constitutional_hybrid_reading (dual-source), and republican_reading (this file, upward flow). Each story carries its own stable epsilon; the family is linked through affects_constraints so drift and contamination propagate visibly across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
