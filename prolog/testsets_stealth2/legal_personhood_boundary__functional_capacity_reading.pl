% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__functional_capacity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__functional_capacity_reading, []).

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
 *   constraint_id: legal_personhood_boundary__functional_capacity_reading
 *   human_readable: Legal Personhood Boundary — Functional Capacity Reading
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel. The kernel is
 *   the legal personhood boundary — the persisting commitment that a
 *   principled line separates rights-bearing persons from the rest of the
 *   legal world. Three readings compete: a restrictive anthropocentric
 *   reading (personhood limited to born humans with cognitive capacity), a
 *   developmental potentiality reading (personhood begins at conception), and
 *   this file's functional capacity reading (personhood follows demonstrable
 *   cognitive capacity — rationality, sentience, self-awareness — regardless
 *   of species). Per the epsilon-invariance discipline, this file models ONLY
 *   the capacity-indexed arrangement: a boundary administered by assessment
 *   rather than taxonomy, whose beneficiaries are the beings that cross the
 *   demonstration battery and whose cost-bearers include cognitively atypical
 *   humans, property holders in animals, and the sub-threshold animals whose
 *   exclusion the criterion renders principled rather than arbitrary. The
 *   sibling readings are separate constraints with separate epsilon values:
 *   the restrictive reading authors epsilon for a species-gated arrangement
 *   (its cost-bearing set includes all non-humans; its beneficiary set is
 *   humanity-at-birth), and the developmental reading authors epsilon for a
 *   conception-origin arrangement (embryos protected, animals excluded). The
 *   three are linked as a constraint family through
 *   network.affects_constraints. The claim/metric gap is deliberate: the
 *   reading is CLAIMED as tangled_rope — genuine coordination content (a
 *   principled, assessable criterion replacing contested taxonomy) coexisting
 *   with real asymmetric costs — while the metrics independently describe
 *   moderately high, rising extraction; the engine computes per-seat
 *   classifications from the structural data and measures the divergence. KEY
 *   AGENTS (by structural relationship): -
 *   capacity_qualifying_nonhuman_animals: Primary beneficiary
 *   (powerless/trapped) — great apes, cetaceans, elephants whose demonstrated
 *   capacities would carry them across the boundary -
 *   animal_rights_advocacy_movements: Operational beneficiary
 *   (organized/mobile) — litigants and campaigners whose standing and
 *   resources scale with the criterion -
 *   severely_cognitively_impaired_humans: Primary human cost-bearer
 *   (powerless/trapped) — protection converted from categorical to assessable
 *   - human_infants: Cost-bearer (powerless/trapped) — status deferred until
 *   capacities demonstrate - sentient_subthreshold_animals: Cost-bearer
 *   (powerless/trapped) — exclusion acquires a principled warrant -
 *   animal_use_industries: Property-holder cost-bearer (powerful/constrained)
 *   - artificial_agent_developers: Prospective beneficiary (powerful/mobile)
 *   - religious_and_traditional_authorities: Displaced gatekeeper
 *   (organized/constrained) - disability_rights_organizations: Organized
 *   opposition speaking for atypical-human seats (organized/constrained) -
 *   constitutional_courts: Agenda setter (institutional/analytical) —
 *   administer adoption and enforcement pace - legal_philosophers: Analytical
 *   observer (analytical/analytical)
 *
 * KEY AGENTS:
 *   - capacity_qualifying_nonhuman_animals: Primary beneficiary (powerless/trapped) — beings that pass the battery and would gain standing directly
 *   - animal_rights_advocacy_movements: Operational beneficiary (organized/mobile) — supply the proxies, litigation, and doctrinal leverage
 *   - severely_cognitively_impaired_humans: Primary human cost-bearer (powerless/trapped) — categorical protection becomes an assessment outcome
 *   - human_infants: Cost-bearer (powerless/trapped) — personhood deferred or derivative under the strict reading
 *   - sentient_subthreshold_animals: Cost-bearer (powerless/trapped) — their continued exclusion gains principled justification
 *   - animal_use_industries: Property-holder cost-bearer (powerful/constrained) — titled interests exposed to reclassification
 *   - artificial_agent_developers: Prospective beneficiary (powerful/mobile) — a lawful path to machine moral status
 *   - religious_and_traditional_authorities: Displaced gatekeeper (organized/constrained) — definitional authority over the boundary lost
 *   - disability_rights_organizations: Organized opposition (organized/constrained) — campaign against capacity-gated frameworks
 *   - constitutional_courts: Agenda setter (institutional/analytical) — decide whether and how the criterion enters positive law
 *   - legal_philosophers: Analytical observer (analytical/analytical) — map the readings and audit extensional consequences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.58).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.52).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Legal Personhood Boundary — Functional Capacity Reading").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, 'aac56409-df2c-4d5c-9a75-eef9fd2ed93d').
narrative_ontology:cs_kernel_codification('aac56409-df2c-4d5c-9a75-eef9fd2ed93d', formalized).
narrative_ontology:cs_authority_grounding('aac56409-df2c-4d5c-9a75-eef9fd2ed93d', distributed).
narrative_ontology:cs_reading_relation('aac56409-df2c-4d5c-9a75-eef9fd2ed93d', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('aac56409-df2c-4d5c-9a75-eef9fd2ed93d', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_axiom('aac56409-df2c-4d5c-9a75-eef9fd2ed93d', foundational, cognitive_capacity_sole_personhood_criterion).
narrative_ontology:cs_axiom_status(cognitive_capacity_sole_personhood_criterion, holdable).
narrative_ontology:cs_axiom_grounding('aac56409-df2c-4d5c-9a75-eef9fd2ed93d', cognitive_capacity_sole_personhood_criterion, deontological).
narrative_ontology:cs_axiom('aac56409-df2c-4d5c-9a75-eef9fd2ed93d', secondary, species_neutral_status_assignment).
narrative_ontology:cs_axiom_status(species_neutral_status_assignment, holdable).
narrative_ontology:cs_axiom_grounding('aac56409-df2c-4d5c-9a75-eef9fd2ed93d', species_neutral_status_assignment, deontological).
narrative_ontology:cs_reference_frame('aac56409-df2c-4d5c-9a75-eef9fd2ed93d', capacity_indexed_moral_community).
narrative_ontology:cs_drift_state('aac56409-df2c-4d5c-9a75-eef9fd2ed93d', contemporary_positive_law, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('aac56409-df2c-4d5c-9a75-eef9fd2ed93d', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, capacity_qualifying_nonhuman_animals).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, animal_rights_advocacy_movements).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, severely_cognitively_impaired_humans).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, human_infants).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, sentient_subthreshold_animals).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, animal_use_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, artificial_agent_developers).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, religious_and_traditional_authorities).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, disability_rights_organizations).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, moral_individualism_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, argument_from_marginal_cases).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, evolutionary_continuity_of_mind).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Great apes, cetaceans, and elephants — beings that pass mirror self-recognition, plan tool use, and maintain social self-models. Today they are held as research stock, performance property, or zoo inventory under existing personhood law. Under this reading they would hold legal standing directly: their interests would be represented in court, and their confinement and use would require justification against a rights-holder rather than an owner. They cannot petition, migrate between jurisdictions, or advocate; every gain arrives through human proxies, and nothing they do changes which side of the boundary they sit on.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, capacity_qualifying_nonhuman_animals, beneficiary,
    powerless, biographical, trapped, global).

% Litigation programs, philosophy departments, and campaign organizations that supply the proxies and the arguments. The criterion gives them a doctrinal lever: habeas petitions for chimpanzees, personhood resolutions for great apes, ethics panels citing cetacean self-recognition. Their funding, media standing, and institutional access scale with how seriously institutions take the criterion. They can reframe, redirect, and relocate campaigns across jurisdictions; their commitment is professional and organizational rather than existential.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, animal_rights_advocacy_movements, beneficiary,
    organized, generational, mobile, global).

% Humans with profound intellectual disability, advanced dementia, or severe brain injury. Existing law protects them categorically as persons. Under a demonstrated-capacity standard their status becomes an assessment outcome: guardians must argue from proxies, panels must weigh evidence, and protection becomes something that can be argued down as well as up. They cannot perform on any test battery, cannot exit the category they are placed in, and depend entirely on whoever speaks for them.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, severely_cognitively_impaired_humans, payer,
    powerless, biographical, trapped, global).

% Neonates and toddlers cannot demonstrate rationality, sustained self-awareness, or reasoned preference. Under the strict reading their personhood is deferred or derivative — held in trust until capacities emerge, or secured through arguments about developing potentials that the reading's own logic discounts. Parents and pediatricians become participants in status questions rather than holders of settled protection. The situation resolves biologically for most, but the window in which protection is contingent is real.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, human_infants, payer,
    powerless, immediate, trapped, global).

% Poultry, fish, rodents, and most farmed and laboratory animals: beings that clearly feel pain and distress but fail the full battery of rationality and self-awareness tests. The capacity criterion does not open the boundary for them; it supplies a principled reason the boundary stays closed — their continued use is classified as considered rather than prejudicial. Welfare regulation remains their ceiling, and no behavioral change they could make alters the classification.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, sentient_subthreshold_animals, payer,
    powerless, biographical, trapped, global).

% Agribusiness, pharmaceutical and cosmetic testing operations, marine parks, and research universities holding legal title, contracts, and capital sunk into the use of animals. Reclassification of even the qualifying few threatens precedent that reaches their whole portfolio: if a chimpanzee is a person, research titles over apes are void and the category of animal property is destabilized. They respond with lobbying, litigation, and substitution research; exit means writing off sunk capital and rebuilding processes around alternatives, which is slow and expensive but not impossible for the largest firms.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, animal_use_industries, payer,
    powerful, generational, constrained, global).

% Firms and laboratories building increasingly capable machine systems. A capacity-indexed boundary gives their artifacts a lawful path toward moral and legal standing: demonstrate the battery, cross the boundary. Today's systems fail the tests, but the criterion converts a metaphysical dead end into an engineering target, and developer investment in capability demonstrations doubles as status-building. They are mobile — capital, talent, and jurisdiction-shopping are all available — and their interest in the criterion is prospective rather than present.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, artificial_agent_developers, beneficiary,
    powerful, generational, mobile, global).

% Institutions that locate personhood in ensoulment, imago dei, natural-kind membership, or communal ritual status rather than in psychological performance. The capacity criterion displaces their definitional authority over who counts: a boundary they have administered for millennia becomes a laboratory measurement. They bear cultural and political costs — loss of veto power over bioethics, displacement of doctrine by psychometrics — and they mount the oldest and deepest opposition to the reading. Their commitments span generations and admit no jurisdictional exit.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, religious_and_traditional_authorities, payer,
    organized, civilizational, constrained, global).

% Advocacy groups speaking for people whose capacities are atypical, fluctuating, or diminishing. They read the criterion as converting their constituents' protection into a test some will fail, and they campaign against capacity-gated frameworks in legislation, bioethics panels, and court amicus briefs. They are organized and professionally staffed, anchored in particular national legal systems, and unable to exit the dispute because the criterion targets their constituents' status directly.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, disability_rights_organizations, payer,
    organized, biographical, constrained, national).

% Courts and constitutional bodies that decide whether and how the capacity criterion enters positive law: granting or denying habeas petitions for captive chimpanzees, weighing personhood resolutions, setting evidentiary standards for capacity claims. They administer the boundary's enforcement pace — each ruling either hardens the species-line settlement or cracks it. They hold the pen on implementation; their horizon is institutional and intergenerational, and they answer to no market exit.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Scholars mapping the boundary's competing criteria, running the argument-from-marginal-cases analysis, and auditing each reading's extensional consequences. They collect no rents and bear no costs; their output is the analytical record the other seats argue over.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, legal_philosophers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__functional_capacity_reading, animal_rights_advocacy_movements).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__functional_capacity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a single, assessable criterion for allocating legal and moral status across all beings, replacing taxonomy-based line-drawing: legislatures, courts, and review bodies can adjudicate status disputes by evaluating capacities rather than consulting species categories, and advocates gain a common evidentiary currency (comparative-cognition results) for status claims.
% TRANSFER_FUNCTION: Moves legal standing and protection toward beings that demonstrate the capacity battery — a handful of non-human species today, prospectively artificial agents — and moves definitional authority away from species-based custom and theological gatekeeping toward assessment institutions. Simultaneously it converts the protection of humans who cannot demonstrate the battery from settled entitlement into contestable status, and exposes titled interests in animals to reclassification.
% ABSENT_VOICES: The beings whose status is decided cannot speak: capacity-qualifying animals act only through human proxies, sub-threshold animals have no representation at all, and severely cognitively impaired humans are voiced by guardians and advocacy organizations who dispute the criterion's premises rather than merely its application. The demonstration standard is therefore set entirely by parties on the arguing side of the boundary — an absence that shapes whose capacities get measured and how.
% DISAPPEARANCE_RATIONALE: Material arrangements would scarcely move overnight — farms, laboratories, and zoos operate under species-line law that this reading has not displaced. But a dense discursive and litigative architecture depends on it: habeas programs, great-ape personhood campaigns, AI moral-status discourse, and a large scholarly literature would lose their organizing criterion, and the sibling readings would absorb the contested terrain. Proponents assert the moral-legal trajectory depends on it; opponents assert nothing of substance turns on it; the parties genuinely dispute which world we are in.
% FOUNDING_PROBLEM: After Darwin, the biological discontinuity that supposedly justified an exclusively human personhood boundary collapsed: if minds differ in degree across species, a boundary drawn at species membership looks arbitrary, and the argument from marginal cases shows it protects some humans on taxonomy alone while excluding demonstrably sophisticated animals. The reading was built to solve that arbitrariness — to re-ground personhood in properties rather than pedigree.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: comparative-cognition researchers (including the Cambridge Declaration on Consciousness signatories) attest the empirical continuity the problem rests on; bioethicists critical of animal advocacy — including disability scholars who reject this reading's solution — nonetheless treat the arbitrariness problem itself as real and unresolved; religious bodies attest the problem while rejecting the capacity solution. No major participant in the dispute denies that the founding problem exists; the contest is over solutions.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__functional_capacity_reading, contested).
narrative_ontology:founding_problem_status(legal_personhood_boundary__functional_capacity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legal_personhood_boundary__functional_capacity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__functional_capacity_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__functional_capacity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__functional_capacity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is substantial but hybrid: the criterion genuinely solves a coordination problem — status allocation by assessable property instead of contested taxonomy — yet its operation concentrates real costs on identifiable seats: atypical humans whose protection becomes assessable, property holders facing reclassification of titled interests, and sub-threshold animals whose exclusion acquires a principled warrant it previously lacked. Suppression (0.52) reflects the enforcement reality: implementation requires overriding entrenched property law and constitutional settlements and displacing theological and customary gatekeepers, but day-to-day coercive intensity remains modest because adoption is thin. Theater (0.45) is high and rising: declarations, symposia, and symbolic rulings outnumber implemented boundary changes in the later interval. Accessibility collapse is low (0.30) because the alternatives — the sibling readings, welfare-regulation approaches, relational accounts — remain fully live; this is a contested construct, not a closed trap. Resistance is high (0.75): disability coalitions, religious authorities, and agricultural and research interests actively defend the species-line settlement. Identity_coordination is declared because the arrangement's primary function is membership-boundary maintenance against evolving criteria; the known gaming risk of identity framing as extraction cover is monitored through the capacity_demonstration_governance omega. The measurement series run on one shared time grid (points 0-50 at decade steps) with all three tracked metrics authored at every point. Suppression_requirement is authored because the story specifically tracks enforcement-capacity growth: from no machinery in the pure-theory era to active litigation programs, assessment standards, and courts policing property contests. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine (by directionality and spatial scope). Fixing_cost is prohibitive: no institution can un-invent the comparative-cognition record or the argument from marginal cases, and suppressing the criterion would require policing science and philosophy at a cost exceeding any benefit of closure. The claimed type and the metrics are independent authored facts: I claim tangled_rope because coordination, asymmetric costs, and required enforcement are all structurally present; the engine computes each seat's type and any divergence from the claim is the datum.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the capacity-qualifying-animal seat the arrangement is emancipation — a boundary opening that converts property into standing, with effective extraction near zero or inverted into subsidy. From the animal_use_industries seat the same structure is uncompensated reclassification risk riding on litigation — high effective extraction. From the severely-impaired-human seat the criterion is a threat dressed as principle: the very feature advertised as fairness (capacity-neutrality) is the mechanism that makes their protection conditional. From the advocacy seat it is a coordination achievement that is also their resource base. Same nominal instrument, four different experienced constraints — the engine computes this divergence from power, exit, and directional position; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: capacity-qualifying animals (trapped, powerless) sit nearest the full-beneficiary end — the boundary opening subsidizes them entirely; advocacy movements collect standing and resources and sit low; AI developers hold a prospective, mobile beneficiary position. Victim declarations drive high directionality: impaired humans and infants (trapped, powerless) sit nearest the full-target end — they bear the criterion's sharpest costs with no exit; sub-threshold animals likewise, with the added structural fact that the criterion legitimizes rather than relieves their exclusion; animal-use industries bear high costs but hold power and partial substitution exits, moderating their effective position; religious authorities bear definitional-authority losses with no jurisdictional exit. Spatial scope is global for nearly every seat — the criterion travels with the comparative-cognition literature — which amplifies verification difficulty and hence effective extraction for target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-Darwinian arbitrariness of the species line) is live and corroborated by sources outside the benefiting parties, so no obsolescence flag is warranted: the mismatch consumer reads founding_problem_status=live against disappearance_verdict=contested and finds no dead-mandate/world-rearranges mismatch. Mandatrophy discipline prevents two mislabels here. First, it prevents reading the arrangement as a pure advocacy rope: the proponent seat experiences genuine coordination, but the impaired-human and sub-threshold-animal seats experience unconditional costs, and both halves are load-bearing in one structure that requires active enforcement — the tangled-rope signature, not a rope with noise. Second, it prevents premature pure-extraction labeling from the target seats alone: the criterion's coordination function is real (assessment beats taxonomy as a dispute-resolution technology) and its beneficiary set is not empty, so a pure-extraction verdict would erase the half of the structure that explains why sophisticated actors defend it. If the founding problem were ever resolved — a settled, widely accepted status criterion — this arrangement would decay toward theatrical maintenance of a settled question or dissolve outright; the measurements' rising theater ratio is the early indicator to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story instantiates only the functional_capacity_reading of the legal_personhood_boundary kernel; how would the victim set, beneficiary set, and epsilon change under the sibling readings?',
    'Generate the sibling stories (restrictive_anthropocentric_reading, developmental_potentiality_reading) and compare computed classifications; the delta appears in victim-set membership and epsilon across the family, not inside this file.',
    'Under the restrictive reading, every non-human animal remains outside the protected class regardless of demonstrated capacity (no animal beneficiaries at all); under the developmental reading, embryos and fetuses enter the protected class while capacity-qualifying animals stay excluded. This file''s epsilon is valid only for the capacity-indexed arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame routing: one reading of a three-reading kernel; sibling deltas live in sibling files.').

omega_variable(
    threshold_placement_margin_cases,
    'Where exactly does the demonstrable-capacity threshold sit, and which humans (infants, profound intellectual disability, late-stage dementia) fall on which side?',
    'Comparative-psychology batteries applied consistently across species and human populations, plus legislative drafting choices between binary and graduated status.',
    'A graduated or relational modification dissolves the strongest objection and moves the arrangement toward broader coordination; a strict binary entrenches the impaired-human and infant cost-bearing classes and pushes payer-seat classifications toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_placement_margin_cases, empirical, 'Threshold location determines the size and composition of the human cost-bearing classes.').

omega_variable(
    capacity_demonstration_governance,
    'Who administers the capacity test battery, and is the demonstration standard set by parties with a stake in its breadth?',
    'Institutional analysis of assessment bodies (courts, ethics boards, certification regimes) audited against independent comparative-cognition literature.',
    'A stringent battery administered by interested parties reduces beneficiaries to a token few and lends principled cover to expanded sub-threshold use; a lenient independent battery sweeps in most vertebrates and transforms the arrangement''s coordination profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_demonstration_governance, conceptual, 'Governance of the demonstration standard controls who crosses the boundary.').

omega_variable(
    implementation_transition_terms,
    'Would implementation proceed through compensated, phased transition or through uncompensated reclassification of titled property in animals?',
    'Statutory design in jurisdictions adopting the criterion: compensation clauses, phase-in schedules, and litigation outcomes over existing titles.',
    'Compensated transition preserves the coordination half and keeps the industry seat''s costs moderate; uncompensated confiscation maximizes industry-seat costs and pushes the arrangement toward pure extraction from the property-holder seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_transition_terms, preference, 'Transition design determines whether property holders experience the cost as coordination or as expropriation.').

omega_variable(
    synthetic_agent_entry,
    'Will artificial systems demonstrably satisfy the capacity battery, and does their entry stabilize the criterion or expose its administrators as gatekeepers?',
    'Capability evaluation against the same batteries applied to animals, adjudicated by bodies independent of developer firms.',
    'Entry expands the beneficiary set dramatically and forces threshold renegotiation; refusal despite demonstrated capacity would reveal the administration as serving incumbent interests and drive drift toward inertial or extractive operation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(synthetic_agent_entry, empirical, 'Synthetic-agent entry tests whether the criterion is administered even-handedly across substrates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(lega_tr_t10, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(lega_tr_t20, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(lega_tr_t30, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(lega_tr_t50, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(lega_be_t10, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(lega_be_t20, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(lega_be_t30, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement(lega_be_t50, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(lega_su_t10, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 10, 0.29).
narrative_ontology:measurement(lega_su_t20, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(lega_su_t30, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 40, 0.49).
narrative_ontology:measurement(lega_su_t50, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__functional_capacity_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, developmental_potentiality_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'legal personhood' decomposes into three structurally distinct readings of one kernel (legal_personhood_boundary): restrictive_anthropocentric_reading (species-and-birth gate), developmental_potentiality_reading (conception-origin gate), and this file's functional_capacity_reading (demonstrated-capacity gate). Each reading emits a separate constraint with its own epsilon, victim set, and beneficiaries; they form a constraint family linked through affects_constraints. The upstream empirical substrate (comparative-cognition findings) feeds this reading and pressures the restrictive sibling; the developmental sibling draws on a separate potentiality tradition. Epsilon differs across the family because the referent arrangement differs: this file authors epsilon for the capacity-indexed boundary as this reading holds it, not for the species-gated or conception-origin arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
