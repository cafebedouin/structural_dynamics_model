% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__individual_right_reading, []).

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
 *   constraint_id: second_amendment_text__individual_right_reading
 *   human_readable: Second Amendment Individual-Right Reading (Post-Heller/Bruen Regime)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the second_amendment_text kernel:
 *   the individual-right reading, under which the amendment's operative
 *   clause guarantees a personal right to keep and bear arms independent of
 *   militia service, with personal self-defense as the core protected
 *   activity. The standing arrangement under contest — and therefore the
 *   epsilon referent — is the post-Heller/post-Bruen regime as it actually
 *   operates: a judicially enforced constitutional floor that invalidates
 *   contrary firearms regulation, shields a large protected possession class,
 *   sustains a commercially dependent industry, and imposes categorical
 *   disarmament with criminal liability on defined classes (persons with
 *   felony convictions; persons convicted of misdemeanor domestic-violence
 *   offenses). The two sibling readings (collective_security_reading,
 *   originalist_civic_virtue_reading) are separate constraint files, not
 *   positions inside this one; per the epsilon-invariance principle each
 *   carries its own epsilon, victim set, and classification, and the family
 *   is linked through network.affects_constraints. The claim and the metrics
 *   are independent authored facts: the tangled_rope claim reflects the
 *   structure (genuine liberty-delivery function PLUS asymmetric class burden
 *   PLUS mandatory active enforcement), while the metrics describe observed
 *   operation; where the engine computes divergent per-seat types, that
 *   divergence is the datum.
 *
 * KEY AGENTS:
 *   - individual_gun_owners: Primary beneficiary (organized/identity_locked) — holds the protected liberty; for a large subset ownership is culturally fused, deepening attachment
 *   - firearm_industry: Concentrated commercial beneficiary (institutional/arbitrage) — collects the market-stability rent the constitutional shield provides
 *   - felony_convicted_disarmed_class: Primary target (powerless/trapped) — bears lifetime categorical disarmament and fresh criminal liability
 *   - domestic_abuser_disarmed_class: Secondary target (powerless/trapped) — bears the misdemeanor-triggered federal prohibition
 *   - federal_judiciary: Agenda-setter (institutional/constrained) — administers the kernel through text-history-tradition adjudication
 *   - state_legislatures: Cost-bearing regulator (institutional/constrained) — legislates inside a floor it does not control; statutes voidable after passage
 *   - gun_violence_prevention_movement: Excluded voice (organized/mobile) — contemporary empirical expertise sits outside the governing adjudicative method
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, 0.52).
domain_priors:suppression_score(second_amendment_text__individual_right_reading, 0.64).
domain_priors:theater_ratio(second_amendment_text__individual_right_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_right_reading, "Second Amendment Individual-Right Reading (Post-Heller/Bruen Regime)").
narrative_ontology:topic_domain(second_amendment_text__individual_right_reading, "constitutional/political").

domain_priors:requires_active_enforcement(second_amendment_text__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_right_reading, 'b21fb795-4095-443c-b64f-d469df5af3bd').
narrative_ontology:cs_kernel_codification('b21fb795-4095-443c-b64f-d469df5af3bd', fixed_text).
narrative_ontology:cs_authority_grounding('b21fb795-4095-443c-b64f-d469df5af3bd', lineage).
narrative_ontology:cs_interpretation_layer_present('b21fb795-4095-443c-b64f-d469df5af3bd').
narrative_ontology:cs_reading_relation('b21fb795-4095-443c-b64f-d469df5af3bd', second_amendment_text__collective_security_reading, forecloses).
narrative_ontology:cs_reading_relation('b21fb795-4095-443c-b64f-d469df5af3bd', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('b21fb795-4095-443c-b64f-d469df5af3bd', foundational, self_defense_is_core_protected_activity).
narrative_ontology:cs_axiom_status(self_defense_is_core_protected_activity, holdable).
narrative_ontology:cs_axiom_grounding('b21fb795-4095-443c-b64f-d469df5af3bd', self_defense_is_core_protected_activity, deontological).
narrative_ontology:cs_axiom('b21fb795-4095-443c-b64f-d469df5af3bd', foundational, right_independent_of_militia_service).
narrative_ontology:cs_axiom_status(right_independent_of_militia_service, holdable).
narrative_ontology:cs_axiom_grounding('b21fb795-4095-443c-b64f-d469df5af3bd', right_independent_of_militia_service, conventional).
narrative_ontology:cs_reference_frame('b21fb795-4095-443c-b64f-d469df5af3bd', preexisting_individual_right_of_self_defense).
narrative_ontology:cs_drift_state('b21fb795-4095-443c-b64f-d469df5af3bd', contemporary_post_bruen_rahimi_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b21fb795-4095-443c-b64f-d469df5af3bd', '').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_right_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, firearm_industry).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, felony_convicted_disarmed_class).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, domestic_abuser_disarmed_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, state_legislatures).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, heller_individual_right_interpretation).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, bruen_text_history_tradition_test).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Roughly a third of households hold firearms whose possession the constitutional guarantee shields from ordinary legislative revision. For a substantial subset, ownership is fused with rural identity, self-conception, and community standing; exit means divesting property and leaving a cultural world, not merely complying with a rule. Well-resourced advocacy organizations litigate and lobby on their behalf.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Manufacturers and dealers sell into a demand base stabilized by the constitutional shield: regulatory erosion of the market is foreclosed at the federal floor, and the reading's expansion (carry rights, invalidation of restrictions) enlarges the addressable market. The industry funds much of the litigation and advocacy that maintains the reading, and can shift product lines, export markets, or state domiciles if any single jurisdiction turns hostile.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, firearm_industry, beneficiary,
    institutional, biographical, arbitrage, global).

% Tens of millions of Americans with felony convictions face federal and state possession bans attaching for life in most jurisdictions. Possession is a fresh criminal offense regardless of the underlying conviction's severity; relief runs through expungement or pardon processes that are rare, slow, and state-dependent. The class has no organized representation in the interpretive conversation and, in many states, has also lost the franchise.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, felony_convicted_disarmed_class, payer,
    powerless, biographical, trapped, national).

% Persons convicted of misdemeanor domestic-violence offenses fall under the federal prohibition; the category triggers at a lower offense level than the felony class and attaches without regard to sentence served. Recent Supreme Court adjudication upheld the prohibition's constitutionality within the reading. Exit from the category is effectively nonexistent once the conviction stands.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, domestic_abuser_disarmed_class, payer,
    powerless, biographical, trapped, national).

% Life-tenured judges administer the kernel: they decide which regulations survive the text-history-tradition methodology, strike down inconsistent statutes, and define the right's outer bounds. Their interpretive moves are bounded by precedent, the confirmation process, and circuit politics; they cannot exit the role, and each appointment shifts the constraint's operating parameters for decades.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% State legislatures enact most day-to-day firearms regulation but do so inside a constitutional floor they do not control: statutes on carry licensing, assault weapons, and large-capacity magazines have been invalidated after enactment, at real cost in passed-and-struck legislative labor and litigation. They still set the state-level agenda within the floor (permit regimes, red-flag laws), but their regulatory output is voidable by a court they did not appoint.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, state_legislatures, payer,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__individual_right_reading, state_legislatures, agenda_setter).

% Public-health researchers, epidemiologists, and survivor-led organizations hold a large evidence base on firearms injury, but the governing adjudicative methodology weighs founding-era text and history rather than contemporary empirical findings, leaving their expertise structurally outside the conversation that decides what regulation is permissible. They pursue ballot initiatives, state legislation, and strategic litigation instead, shifting venues when one closes.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, gun_violence_prevention_movement, excluded,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__individual_right_reading, firearm_industry).
narrative_ontology:fixing_cost_class(second_amendment_text__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single, nationally stable rule for who may possess arms and removes that rule from ordinary political renegotiation: owners receive settled expectations, states receive a known floor beneath which regulation cannot fall, and last-resort self-protection is assigned to households rather than awaited from a state that cannot be present at every threat.
% TRANSFER_FUNCTION: Moves regulatory authority upward from state and local governments to the federal constitutional floor as adjudicated by the courts; moves categorical legal disability (disarmament plus criminal liability) onto defined classes of persons; moves market stability to the firearm industry; and moves final-line security responsibility onto individual households.
% ABSENT_VOICES: Public-health researchers and gun-violence survivors are methodologically excluded from text-history-tradition adjudication, which consults founding-era sources rather than contemporary evidence. The disarmed classes themselves have no seat in the interpretive conversation, compounded in many states by felon disenfranchisement. Deeper in the genealogy: the founding-era conversation that the methodology treats as authoritative structurally excluded enslaved people, Indigenous nations, and women from the armed civic class whose supposed right is being vindicated.
% DISAPPEARANCE_RATIONALE: If the individual-right guarantee vanished overnight, state legislatures would pass licensing, carry-restriction, and category-ban statutes within a single session; tens of millions of lawfully possessed firearms would enter regulatory gray zones; the industry would contract against a shrinking protected market; and the political coalitions organized around the issue would realign around ordinary statutory politics. The arrangement is load-bearing for a very large status quo.
% FOUNDING_PROBLEM: A newly independent republic without a professional standing army needed a credible mechanism of common defense and a counterweight to federal military coercion; the founding generation, drawing on the 1689 English settlement's memory of Stuart disarmament, secured an armed citizenry organized through the militia. Personal self-defense was part of the background inheritance but not the organizing problem.
% FOUNDING_PROBLEM_CORROBORATION: The documentary record (Federalist No. 46, state ratification debates, the 1689 English Bill of Rights lineage) and academic historians outside the beneficiary coalition corroborate that the founding problem existed and was militia-centered. No party outside the gun-rights coalition attests that the anti-standing-army problem remains operationally live; liveness claims come from within the beneficiary coalition, while militia-centered historians attest the founding problem is historically real but institutionally obsolete, its function absorbed by the National Guard and the permanent military establishment.
narrative_ontology:disappearance_verdict(second_amendment_text__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_text__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__individual_right_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: the arrangement delivers a real, valued liberty to tens of millions while imposing categorical, criminal-enforced disability on defined classes and seizing regulatory authority from fifty state legislatures; the genuine delivery keeps epsilon moderate rather than high, the class burdens and authority transfer keep it well above rope territory. Suppression 0.64: persistence depends on actively striking down the regulatory alternative space (may-issue licensing, category bans), yet suppression is incomplete — shall-issue permitting, background checks, and sensitive-place rules survive, so alternatives are narrowed, not eliminated. Theater 0.33: the militia preamble is functionally dead in this reading (demoted to prefatory status), and the text-history-tradition methodology carries a substantial performative component (founder-worship pageantry, analogical historical argument that often decorates outcome-driven reasoning), but the core enforcement — statutes actually falling, possession actually protected — is real. Accessibility_collapse 0.50: once Bruen's test is understood, novel regulatory forms become foreseeably futile and the option space partially collapses, but a workable regulatory core persists. Resistance 0.78: the constraint meets continuous, organized counter-mobilization — post-shooting legislative waves, Bruen-response statutes, ballot measures, dueling litigation — which is the signature of a contested construct, not a natural law. The temporal series run on one shared eight-point grid (2008-2026) so every tracked metric is authored at every examined time point; the 2026 endpoints are marked projected. The suppression_requirement series is authored deliberately: this story specifically tracks enforcement-capacity build-up — the machinery (THT litigation wave, historical-research cottage industry, circuit enforcement) was constructed over the interval, peaked at Bruen, and tempered modestly after Rahimi.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the individual_gun_owners seat the arrangement is a near-absolute shield — a liberty guarantee doing exactly what it promises, with identity fusion making the protected status constitutive rather than instrumental. From the disarmed-class seats the same structure operates as a categorical exclusion machine backed by criminal process. From the state_legislatures seat it is an authority seizure: democratically enacted law voided by an unelected tribunal applying a historical method. From the federal_judiciary seat it is interpretive sovereignty — the constraint IS the court's practice. From the firearm_industry seat it is a market-protection device. The engine computes these per-seat classifications from power, exit, and role; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation and no overrides are needed because role plus exit already discriminates every seat. individual_gun_owners derive near the beneficiary pole (d low); their identity_locked exit deepens the incumbency of that position — identity lock amplifies whichever side of the structure an agent occupies, and for beneficiaries it entrenches the subsidy. firearm_industry, with arbitrage-grade exit, sits nearest the full-beneficiary end: it captures the arrangement's gains while bearing the least exposure. The two disarmed classes derive near the full-target pole (d high): they are the named victims, trapped, with the burden attached for life. state_legislatures derive target-side despite their agenda-setting secondary role — they bear voided-statute costs with no exit from the constitutional floor. federal_judiciary, as agenda_setter with no beneficiary or victim declaration, takes the canonical fallback, which is appropriate: the judiciary administers rather than collects, though commentary notes the mild institutional interest in an expanded docket.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — assuring an armed militia as counterweight to standing armies — has been institutionally absorbed (National Guard, permanent military) and is operationally dead; the reading survived by substituting a new mandate (personal self-defense) rather than sunsetting. This is mandate-substitution, not resolved mandatrophy: the arrangement persists on a replaced justification, which is why founding_problem_status is authored contested rather than dead, and why no mandatrophy_resolved flag is asserted. The tangled_rope classification prevents the two standard mislabelings: reading the arrangement as pure rope ignores the categorical class burdens and the concentrated industry capture riding on the coordination function; reading it as a snare ignores the genuine, widely-valued liberty delivered to the protected class. The status-contested x world_rearranges cell flags the substitution for the mismatch consumer without asserting a zombie verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the second_amendment_text kernel (individual_right_reading). Would adoption of a sibling reading — collective_security_reading or originalist_civic_virtue_reading — change the structural classification?',
    'Doctrinal evolution at the Supreme Court (composition change altering the governing reading), a constitutional amendment, or a sustained supermajoritarian reversal of the interpretive settlement.',
    'Under collective_security_reading the individual-beneficiary structure dissolves entirely (possession becomes contingent on civic/militia participation and state regulation is freed); under originalist_civic_virtue_reading the right-holder set becomes duty-bound citizens rather than rights-bearing individuals, changing who counts as beneficiary and restoring a civic-obligation condition this reading lacks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame omega: classification is indexed to one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    civilian_armament_externality_sign,
    'Does widespread civilian armament produce net protective externalities (deterrence) or net violence costs for the unarmed public?',
    'Quasi-experimental criminology: shall-issue and permitless-carry natural experiments across states, disaggregated by offense type and demographic stratum, with pre-registered designs to escape the published-literature impasse.',
    'If net protective, the unarmed public is a silent beneficiary and effective extraction drops further below the authored scalar; if net violent-cost, a large unbilled cost-bearer exists outside the authored victim set and effective extraction rises materially. This is the principal empirical fault line separating this reading from its siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_armament_externality_sign, empirical, 'Sign of the diffuse externality from civilian armament — unresolved in the literature and decisive for the arrangement''s net extraction.').

omega_variable(
    disarmed_class_burden_characterization,
    'Is the categorical disarmament of the felony and domestic-abuser classes an extraction imposed by the arrangement, or a legitimate condition-of-the-right analogous to any rights forfeiture following criminal process?',
    'Conceptual analysis within the reading''s own framework: compare the disarmament''s procedural character (collateral consequence attaching without individualized assessment) against the reading''s own criteria for legitimate limitation; track whether restoration pathways (expungement, pardon) function at meaningful scale.',
    'If characterized as legitimate condition, the authored victim set shrinks and epsilon falls toward rope territory; if characterized as extraction, the asymmetry between protected class and burdened class sharpens and the payer seats compute nearer snare-type treatment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disarmed_class_burden_characterization, conceptual, 'Whether the disarmed classes bear extraction or legitimate limitation — the pivotal conceptual input to the asymmetry half of the tangled-rope structure.').

omega_variable(
    founding_mandate_liveness,
    'Does the founding problem (militia-based counterweight to standing armies) remain live in any operationally meaningful sense, or has the reading fully substituted personal self-defense as a new mandate on a dead foundation?',
    'Institutional analysis: whether any plausible scenario exists in which the armed-citizenry mechanism performs its founding counterweight function given the modern military balance; comparative assessment against the National Guard''s absorption of the militia function.',
    'If the founding mandate is dead and substitution complete, the arrangement is mandate-substituted rather than transitional — no scaffold character, elevated long-run inertia risk; if any live residual exists, part of the arrangement''s justification remains connected to its origin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_mandate_liveness, conceptual, 'Liveness of the founding mandate versus mandate-substitution — governs obsolescence and inertia assessment.').

omega_variable(
    text_history_tradition_durability,
    'Will the text-history-tradition methodology survive as the governing adjudicative method, or is the Rahimi-era softening (principle over holistic historical survey) the leading edge of methodological decay?',
    'Track the Supreme Court''s next several Second Amendment merits decisions and the circuits'' handling of Bruen-derived cases: consolidation of THT versus migration to means-end or interest-balancing hybrids.',
    'Methodological durability sustains the rising suppression trajectory (more regulatory alternatives collapse); methodological decay would flatten suppression_requirement and reopen the collapsed option space, moving the arrangement back toward rope-like operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_history_tradition_durability, empirical, 'Durability of the enforcement methodology that drives the suppression trajectory after 2022.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_right_reading, 2008, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__individual_right_reading, theater_ratio, 2008, 0.2).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_text__individual_right_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement_basis(seco_tr_t2010, observed).
narrative_ontology:measurement(seco_tr_t2013, second_amendment_text__individual_right_reading, theater_ratio, 2013, 0.24).
narrative_ontology:measurement_basis(seco_tr_t2013, observed).
narrative_ontology:measurement(seco_tr_t2016, second_amendment_text__individual_right_reading, theater_ratio, 2016, 0.26).
narrative_ontology:measurement_basis(seco_tr_t2016, observed).
narrative_ontology:measurement(seco_tr_t2019, second_amendment_text__individual_right_reading, theater_ratio, 2019, 0.27).
narrative_ontology:measurement_basis(seco_tr_t2019, observed).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_text__individual_right_reading, theater_ratio, 2022, 0.32).
narrative_ontology:measurement_basis(seco_tr_t2022, observed).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_text__individual_right_reading, theater_ratio, 2024, 0.34).
narrative_ontology:measurement_basis(seco_tr_t2024, observed).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_text__individual_right_reading, theater_ratio, 2026, 0.33).
narrative_ontology:measurement_basis(seco_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__individual_right_reading, base_extractiveness, 2008, 0.4).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2010, second_amendment_text__individual_right_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement_basis(seco_be_t2010, observed).
narrative_ontology:measurement(seco_be_t2013, second_amendment_text__individual_right_reading, base_extractiveness, 2013, 0.46).
narrative_ontology:measurement_basis(seco_be_t2013, observed).
narrative_ontology:measurement(seco_be_t2016, second_amendment_text__individual_right_reading, base_extractiveness, 2016, 0.48).
narrative_ontology:measurement_basis(seco_be_t2016, observed).
narrative_ontology:measurement(seco_be_t2019, second_amendment_text__individual_right_reading, base_extractiveness, 2019, 0.5).
narrative_ontology:measurement_basis(seco_be_t2019, observed).
narrative_ontology:measurement(seco_be_t2022, second_amendment_text__individual_right_reading, base_extractiveness, 2022, 0.56).
narrative_ontology:measurement_basis(seco_be_t2022, observed).
narrative_ontology:measurement(seco_be_t2024, second_amendment_text__individual_right_reading, base_extractiveness, 2024, 0.54).
narrative_ontology:measurement_basis(seco_be_t2024, observed).
narrative_ontology:measurement(seco_be_t2026, second_amendment_text__individual_right_reading, base_extractiveness, 2026, 0.52).
narrative_ontology:measurement_basis(seco_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t2008, second_amendment_text__individual_right_reading, suppression_requirement, 2008, 0.45).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2010, second_amendment_text__individual_right_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement_basis(seco_su_t2010, observed).
narrative_ontology:measurement(seco_su_t2013, second_amendment_text__individual_right_reading, suppression_requirement, 2013, 0.55).
narrative_ontology:measurement_basis(seco_su_t2013, observed).
narrative_ontology:measurement(seco_su_t2016, second_amendment_text__individual_right_reading, suppression_requirement, 2016, 0.58).
narrative_ontology:measurement_basis(seco_su_t2016, observed).
narrative_ontology:measurement(seco_su_t2019, second_amendment_text__individual_right_reading, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement_basis(seco_su_t2019, observed).
narrative_ontology:measurement(seco_su_t2022, second_amendment_text__individual_right_reading, suppression_requirement, 2022, 0.68).
narrative_ontology:measurement_basis(seco_su_t2022, observed).
narrative_ontology:measurement(seco_su_t2024, second_amendment_text__individual_right_reading, suppression_requirement, 2024, 0.66).
narrative_ontology:measurement_basis(seco_su_t2024, observed).
narrative_ontology:measurement(seco_su_t2026, second_amendment_text__individual_right_reading, suppression_requirement, 2026, 0.64).
narrative_ontology:measurement_basis(seco_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__originalist_civic_virtue_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, lautenberg_domestic_abuser_prohibition).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Second Amendment' covers three structurally distinct constraints — one per reading of the shared kernel second_amendment_text. This file instantiates the individual_right_reading (epsilon 0.52; victims = categorically disarmed classes; beneficiaries = individual owners plus industry). The collective_security_reading file carries a different victim structure (regulated-away individual possession as the cost of collective security) and the originalist_civic_virtue_reading file a different beneficiary structure (duty-bound citizen-soldiers). The upstream/downstream edge to lautenberg_domestic_abuser_prohibition records that the categorical disarmament of the domestic-abuser class operates inside and is bounded by this reading's enforcement (upheld in recent adjudication). Each member links the others via affects_constraints; no single story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
