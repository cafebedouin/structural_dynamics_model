% ============================================================================
% CONSTRAINT STORY: woman_female_category__hybrid_contextual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__hybrid_contextual_reading, []).

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
 *   constraint_id: woman_female_category__hybrid_contextual_reading
 *   human_readable: Context-Split Membership Rule for the Woman/Female Category
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   Across sports federations, health systems, and civil registries, a
 *   composite rule has crystallized: membership in the woman/female category
 *   is determined biologically in medical, sporting, and safety-sensitive
 *   contexts, and by declared gender identity in social and legal recognition
 *   contexts. No body designed the composite; each sector adopted the locally
 *   tractable key during the operational crisis of the 2000s-2010s, and the
 *   pieces now constitute a standing arrangement administered case by case.
 *   This file authors THAT arrangement — the context-indexed dual-criteria
 *   rule — as one epsilon-invariant constraint. Per the epsilon-invariance
 *   decomposition principle, the colloquial question 'who counts as a woman?'
 *   is three structurally distinct claims, not one: the monist readings are
 *   authored as sibling files linked through the network edges. Their epsilon
 *   values differ because their victim sets and enforcement structures
 *   differ; this reading's epsilon is authored only for the hybrid rule
 *   itself. KEY AGENTS (by structural relationship): -
 *   sports_governing_bodies: agenda-setter and principal beneficiary
 *   (institutional/arbitrage) — administers bio-keyed eligibility, collects
 *   discretionary authority and the category's commercial integrity -
 *   healthcare_systems_administrators and civil_registry_authorities:
 *   parallel agenda-setters (institutional/arbitrage) — collect avoided-cost
 *   gains in clinical and documentary domains - trans_women_athletes: payer
 *   in bio-keyed sport (powerless/constrained) -
 *   trans_patients_sex_keyed_care: payer in bio-keyed clinical contexts
 *   (powerless/trapped) - trans_adults_recognition_contexts:
 *   beneficiary-payer dual seat (moderate/arbitrage) — gains recognition,
 *   loses bio-keyed standing - sex_based_rights_advocates: payer-beneficiary
 *   dual seat (organized/constrained) — retains bio-keyed provisions, loses
 *   recognition contexts - intersex_people: excluded
 *   (powerless/identity_locked) — sorted by administrative convenience, never
 *   consulted - constitutional_courts: analytical observer
 *   (institutional/analytical) — adjudicates key collisions, sees the full
 *   structure. Assumptions stated: the interval 0-24 approximates 2000-2024,
 *   the period over which the composite hardened from sector-by-sector
 *   improvisation into defended policy. Epsilon's referent is the standing
 *   hybrid arrangement itself, assessed by this reading's own lights —
 *   moderately extractive because real costs fall on both constituencies in
 *   alternating domains while the administrating institutions collect the
 *   peace.
 *
 * KEY AGENTS:
 *   - sports_governing_bodies: agenda-setter and principal beneficiary (institutional power, arbitrage exit) — administers bio-keyed eligibility and collects the arrangement's most concentrated gains
 *   - healthcare_systems_administrators: agenda-setter (institutional, arbitrage) — collects avoided record-rebuild and front-line adjudication costs
 *   - civil_registry_authorities: agenda-setter (institutional, arbitrage) — collects avoided definitional adjudication across mass record processing
 *   - trans_women_athletes: primary payer in bio-keyed sport (powerless, constrained) — excluded from the identity-aligned category, tested and disclosed where others are not
 *   - trans_patients_sex_keyed_care: primary payer in clinical contexts (powerless, trapped) — care need cannot be exited
 *   - trans_adults_recognition_contexts: dual beneficiary/payer seat (moderate, arbitrage) — recognition flows in documentary domains, standing flips at bio-keyed doors
 *   - sex_based_rights_advocates: dual payer/beneficiary seat (organized, constrained) — provisions hold in bio-keyed contexts, definition loses force in recognition contexts
 *   - intersex_people: excluded voice (powerless, identity_locked) — fits neither key, sorted per context by administrative convenience
 *   - constitutional_courts: analytical observer (institutional, analytical) — adjudicates key collisions and periodically collapses the hybrid toward one pole
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, 0.55).
domain_priors:suppression_score(woman_female_category__hybrid_contextual_reading, 0.6).
domain_priors:theater_ratio(woman_female_category__hybrid_contextual_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Context-Split Membership Rule for the Woman/Female Category").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, '2815c6ce-207e-4e7e-86e1-d740b589c70d').
narrative_ontology:cs_kernel_codification('2815c6ce-207e-4e7e-86e1-d740b589c70d', distributed).
narrative_ontology:cs_authority_grounding('2815c6ce-207e-4e7e-86e1-d740b589c70d', distributed).
narrative_ontology:cs_reading_relation('2815c6ce-207e-4e7e-86e1-d740b589c70d', woman_female_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('2815c6ce-207e-4e7e-86e1-d740b589c70d', woman_female_category__gender_identity_reading, forecloses).
narrative_ontology:cs_axiom('2815c6ce-207e-4e7e-86e1-d740b589c70d', foundational, criterion_domain_indexed_legitimacy).
narrative_ontology:cs_axiom_status(criterion_domain_indexed_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2815c6ce-207e-4e7e-86e1-d740b589c70d', criterion_domain_indexed_legitimacy, conventional).
narrative_ontology:cs_axiom('2815c6ce-207e-4e7e-86e1-d740b589c70d', foundational, operational_tractability_overrides_metaphysical_monism).
narrative_ontology:cs_axiom_status(operational_tractability_overrides_metaphysical_monism, holdable).
narrative_ontology:cs_axiom_grounding('2815c6ce-207e-4e7e-86e1-d740b589c70d', operational_tractability_overrides_metaphysical_monism, instrumental).
narrative_ontology:cs_reference_frame('2815c6ce-207e-4e7e-86e1-d740b589c70d', context_indexed_criteria_pluralism).
narrative_ontology:cs_drift_state('2815c6ce-207e-4e7e-86e1-d740b589c70d', contemporary_polarized_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2815c6ce-207e-4e7e-86e1-d740b589c70d', '').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, sports_governing_bodies).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, healthcare_systems_administrators).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, civil_registry_authorities).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_women_athletes).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_patients_sex_keyed_care).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, sex_based_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, trans_adults_recognition_contexts).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, sex_based_rights_advocates).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_adults_recognition_contexts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and periodically revise eligibility rules for female-category competition, currently keyed to biological criteria such as sex registered at birth and serum testosterone ranges. They run the verification machinery (eligibility screenings, appeal panels), defend the framework in arbitration and litigation, and license the category's integrity onward to broadcasters and sponsors. When disputes flare they can shift a rule between contexts or tighten a threshold, and the resulting calm or controversy returns to them as credit or cost. The discretionary authority to adjudicate who may compete is itself a valuable asset they did not hold before the split-rule arrangement.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Operate clinical pathways, screening intervals, ward assignments, and record systems that key certain provisions to sex registered at birth while running parallel identity-affirming pathways elsewhere in the same institutions. Keeping the two keys in separate wards of practice spares them rebuilding record infrastructure around a single criterion and spares frontline staff adjudicating identity claims protocol by protocol. Their exposure is reputational and legal rather than bodily.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, healthcare_systems_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Maintain birth, marriage, and identity-document registries in which legal recognition of gender follows declared identity, while other statutes referencing the same category are read biologically by the courts. The dual-key arrangement lets them process millions of records without settling which criterion the category ultimately tracks. Their recurring cost is litigation whenever the two keys collide in a single case file.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, civil_registry_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Compete in disciplines whose female categories are gated on biological criteria. Under the prevailing rules they are ineligible for the category aligned with their identity; the alternatives are entering male or open categories where prior physical development leaves them uncompetitive, or leaving organized sport altogether. Eligibility review subjects them to testing and disclosure demands other competitors never face. Their athletic careers are short relative to the pace at which the rules change, so waiting out a policy cycle usually costs them the career.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, trans_women_athletes, payer,
    powerless, biographical, constrained, global).

% Move through health systems in which screening schedules, dosing references, ward placement, and some referral pathways key to sex registered at birth. Where the keying is clinically grounded it serves them; where it is administrative they encounter intake friction, compelled disclosure, and occasional denial of identity-congruent accommodation. The need for healthcare cannot be exited — delaying or declining care purchases relief from the categorization at the price of health.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, trans_patients_sex_keyed_care, payer,
    powerless, biographical, trapped, national).

% Hold documents, names, and everyday service access governed by declared identity, which most acquire through comparatively straightforward statutory processes. The same people meet bio-keyed rules at the stadium, in some clinical corridors, and at safety-regulated facilities, so their standing flips depending on which door they enter. Jurisdiction-shopping — acquiring recognition where the process is easiest and exercising it where it is honored — is available to those with the mobility to use it, and blunts some of the cost of the doors that close.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, trans_adults_recognition_contexts, beneficiary,
    moderate, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, trans_adults_recognition_contexts, payer).

% Campaign for provisions — single-sex services, sporting categories, statistical monitoring — defined by biological sex. In bio-keyed contexts their definition prevails and the provisions hold; in identity-keyed recognition contexts the same definition loses official force, and they respond with litigation, consultation submissions, and electoral campaigning to move the boundary back. Disengaging from public institutions is their way out, purchased by abandoning the provisions they exist to defend.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sex_based_rights_advocates, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, sex_based_rights_advocates, beneficiary).

% Have variations in sex characteristics that fit neither criterion cleanly. Each institutional context sorts them by administrative convenience — sometimes into the bio-keyed bucket, sometimes the identity-keyed one — and they had no representation when either rule set was drafted. There is no exiting the body the rules are about; their recourse is case-by-case complaint and litigation.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, intersex_people, excluded,
    powerless, biographical, identity_locked, global).

% Adjudicate collisions between the two keys: whether an equality statute's word 'woman' reads biologically, whether a recognition certificate satisfies a federation's eligibility rule, whether a prison counts as a safety context or a recognition context. They see the whole two-key structure from above, and individual rulings periodically collapse the hybrid toward one pole — as when a apex court reads the category biologically throughout an entire area of law.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__hybrid_contextual_reading, sports_governing_bodies).
narrative_ontology:fixing_cost_class(woman_female_category__hybrid_contextual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives every institution that handles the category a locally administrable membership rule, so sex-relevant functions (clinical protocols, competitive categories, statutory recognition) keep operating while the underlying definitional contest is unresolved. The context index tells each operator which key to use; the coordination problem solved is decision-paralysis across thousands of routine classifications.
% TRANSFER_FUNCTION: Moves categorical standing and the goods attached to it between claimant groups by domain: in bio-keyed contexts standing moves from trans people to the incumbents of the sex-defined category; in identity-keyed contexts it moves from sex-based-provision claimants to identity-declared claimants. Simultaneously it moves decisional burden from all claimants to the administering institutions, which retain it.
% ABSENT_VOICES: Intersex people, whose bodies neither key describes, were absent when both rule sets were drafted and remain outside eligibility panels and registry consultations. Detransitioned and nonbinary people, who bear context-switching costs inside a two-key system built for two constituencies, likewise have no seat. Both sit outside the consultation cycles of the federations, health systems, and registries that maintain the arrangement.
% DISAPPEARANCE_RATIONALE: If the context-split rule vanished overnight, every institution would be forced on the spot to adopt one criterion wholesale: eligibility rosters, registry entries, clinical pathways, and facility rules would all redistribute immediately, followed by a litigation storm as whichever criterion each institution grabbed collided with the expectations of everyone else. Nothing about the arrangement is self-maintaining — the goods attached to the category are allocated by it.
% FOUNDING_PROBLEM: The arrangement was assembled piecemeal to end an operational crisis: with both rival criteria claiming the category, sporting eligibility was decided case-by-case in arbitration, registries issued documents one branch of the state refused to honor, and clinical record systems could not reconcile identity-declared entries with physiology-keyed protocols. Each sector adopted the locally tractable key, and the composite became the standing arrangement.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: appellate judgments and arbitration awards from before the hybrid crystallized document the operational chaos it ended (contradictory eligibility rulings, registry-recognition mismatches, irreconcilable record systems), and professional-body submissions from clinicians and official statisticians attest the record-keeping and protocol problems independently of any administrator's convenience. The administrators' own account of the founding problem is therefore not the only attestation on record.
narrative_ontology:disappearance_verdict(woman_female_category__hybrid_contextual_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__hybrid_contextual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__hybrid_contextual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_female_category__hybrid_contextual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__hybrid_contextual_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__hybrid_contextual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__hybrid_contextual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is tangled_rope because the arrangement possesses BOTH required faces: a genuine coordination function (some administrable criterion must govern thousands of routine classifications in medicine, sport, and law — the monist siblings demonstrate the coordination demand is real, not cover) AND asymmetric extraction through the same structure (each domain generates losers whose surrendered standing funds the administrators' conflict-minimization), held together by active enforcement (eligibility screening, documentation regimes, case-by-case context adjudication, and the litigation apparatus defending the split). Metrics describe observed operation: extractiveness 0.55 — moderate, because neither constituency suffers total exclusion; each loses in one domain what it gains in another, while the institutions skim the decisional burden from both. Suppression 0.60 is a raw structural property, deliberately NOT scaled by power or scope — it reflects the eligibility tests, compelled disclosures, and litigation exposure the enforcement machinery applies, full stop. Theater_ratio 0.35: the allocative work is real, but a growing share of activity is performative — balance rhetoric, consultation exercises whose outcomes precede them, and 'case-by-case review' language that launders predetermined results; it rises as the arrangement stabilizes and must be defended rather than built. Accessibility_collapse 0.50: once the rule is understood, the universal-criterion alternatives remain politically alive but are institutionally foreclosed inside the current framework — you cannot opt out of categorization to obtain healthcare, compete, or hold documents. Resistance 0.68: unusually high and bidirectional — both constituencies litigate and campaign continuously against the halves that bind them, which is the signature of a contested compromise rather than an imposed order. Temporal series run on one shared grid (every tracked metric authored at t = 0,4,8,12,16,20,24) so the engine samples complete rows; the suppression_requirement series is included because the story specifically tracks enforcement-capacity growth (verification protocols, appeals panels, legal-defense units matured over the interval) — a rising enforcement trajectory, not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes per-seat classifications from the structural data, and the seats should diverge sharply. From an administrator seat the arrangement computes as coordination it built and defends: a workable classification order that keeps medicine, sport, and law running amid an unresolved contest. From the trans athlete's seat the bio-keyed half operates as exclusion enforced by testing; from the same person's seat at the registry counter the identity-keyed half operates as ordinary recognition. From the advocates' seat the valence reverses domain by domain. The dual seats are the sharpest divergence: the same structure is subsidy and levy depending on the door. No single authored type could represent these experiences simultaneously — which is why the claim is authored at the story level and the per-seat types are left to the engine's computation from power, exit, and directional position.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The three administrative seats are declared beneficiaries with arbitrage-grade exit — they can re-key criteria between contexts, which is precisely the mechanism of the arrangement — placing them near the beneficiary pole (low d, damped or inverted effective extraction). Trans athletes and patients are declared victims with powerless/constrained/trapped profiles, placing them near the full-target pole (high d, amplified extraction; the patient seat's trapped exit pushes it furthest). Two corrections were needed because the derivation reads single roles and these seats are genuinely dual-positioned: trans_adults_recognition_contexts derives toward low d from its beneficiary role, ignoring its substantial bio-keyed losses, so an override sets the moderate-power atom to 0.62; sex_based_rights_advocates derives toward high d from its payer role, ignoring its retained wins across the large bio-keyed territory, so the organized-power atom is overridden to 0.65. Both overrides touch exactly one seat each — no other stakeholder shares those power atoms. The intersex seat is authored as excluded: commentary-grade presence recording an unconsulted constituency, not a classification input. Receipt: gain_flow names sports_governing_bodies because the arrangement's most concentrated gains — discretionary adjudication authority over the highest-salience category in public life, plus the commercial value of certified category integrity — accrue demonstrably to that seat; the health and registry seats collect thinner avoided-cost gains that the single-seat field cannot jointly record, which is stated here rather than left implicit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sector-level operational paralysis under rival criteria) is live — the underlying definitional contest is unresolved, so the management demand persists, and the arrangement performs real allocative work daily. Mandatrophy is therefore not resolved and no sunset clause exists: nothing in the structure declares itself transitional, even though the equilibrium-versus-transitional omega records the possibility that one of the monist readings eventually captures the whole. The classification discipline cuts both ways here: reading the hybrid as pure rope would erase the domain-shifting victim set — real people surrender standing in one domain per the same rule that grants it in another, and those surrenders fund the administrators' peace. Reading it as pure snare would erase the genuine coordination function — the monist siblings prove that some criterion must govern, so the extraction is not riding on a fictional need. Tangled rope names the actual structure: coordination and asymmetric extraction through one enforced, dual-keyed arrangement, with the extraction's incidence rotating by context rather than resting on a fixed victim class.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading of the woman_female_category kernel (hybrid_contextual_reading). What structurally changes if a sibling reading (sex_biology_reading or gender_identity_reading) is instantiated instead?',
    'Compile and compare the sibling stories'' victim sets, enforcement structures, and epsilon values: the monist readings fix the victim set wholesale (bio reading converts all identity-keyed recognition into the contested good; identity reading converts all bio-keyed contexts into exclusion sites), whereas this reading splits the victim set by domain.',
    'Reading choice determines whether the victim set is fixed or domain-shifting, which drives the classification of every seat; the hybrid''s moderate cross-context epsilon is a property of THIS reading, not of the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer-frame omega: which reading of the category kernel is instantiated, and what the siblings would change.').

omega_variable(
    domain_boundary_enumeration,
    'Which contexts actually fall under ''medical/sports/safety'' versus ''social/legal recognition''? The boundary list is itself the operative constraint — prisons, refuges, changing rooms, chess, darts, and statistical collection each sit ambiguously.',
    'Enumerate institutional adoptions and litigation outcomes, mapping each facility and domain to the criterion its governing body actually applies.',
    'Each domain moved onto the bio side of the line enlarges the trans-side victim set; each moved the other way enlarges the advocate-side victim set. The measured extraction of the whole arrangement swings with boundary placements that are currently settled one institution at a time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_boundary_enumeration, empirical, 'The context partition that defines the rule is unsettled and institution-by-institution contested.').

omega_variable(
    equilibrium_vs_transitional,
    'Is the hybrid arrangement a stable long-run equilibrium or a de facto transitional scaffold awaiting capture by one of the monist readings?',
    'Track jurisdiction-level convergence over a decade: if legal systems collapse monotonically toward a single criterion (as apex-court bio-keyed readings and expanding self-ID statutes respectively pull), the hybrid is transitional; sustained stable pluralism indicates equilibrium.',
    'If transitional, the arrangement has a scaffold-shaped lifecycle ending in capture by a sibling reading; if stable, it is a durable tangled_rope whose extraction profile should be analyzed as a permanent feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equilibrium_vs_transitional, conceptual, 'Whether the context-split is an endpoint or a waystation between monisms.').

omega_variable(
    asymmetric_burden_magnitude,
    'Do bio-keyed exclusions impose greater welfare burdens on trans people than identity-keyed subordination of sex-based provisions imposes on their advocates (or vice versa)?',
    'Paired welfare measurement across both populations: participation rates, service access, documented physical and economic harm attributable to each key in its governing domain.',
    'Demonstrated asymmetry would raise effective extraction for the heavier-loaded seat and undermine the arrangement''s own defense as a fair trade; demonstrated symmetry would strengthen the compromise reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asymmetric_burden_magnitude, empirical, 'Whether the two victim groups'' burden magnitudes are comparable or lopsided.').

omega_variable(
    suppression_structural_vs_withdrawal,
    'How much of the measured suppression is enforced by rules (screening, documentation, litigation exposure) and how much is anticipatory self-withdrawal (targets leaving sport or care pathways before any rule binds them)?',
    'Compare participation and presentation rates in otherwise comparable domains that differ only in active enforcement intensity; persistent absence without enforcement indicates internalized withdrawal.',
    'If much of the suppression is anticipatory, removing the enforcement machinery would not quickly restore access — persistence would exceed the formal structure, and the effective suppression experienced at the target seats exceeds the authored structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_withdrawal, empirical, 'Structural versus internalized component of the suppression load on target seats.').

omega_variable(
    cs_kernel_framing_underdetermination,
    'Is the kernel the category concept itself (as framed here), or the state''s claimed authority to classify persons at all — a legitimacy layer sitting above any category account?',
    'Test the two framings against adjudication practice: if disputes turn on which criterion governs the category, the category-kernel framing fits; if they turn on who may classify whom, the authority-kernel framing fits.',
    'Under the authority-kernel framing the commitment-system pattern changes shape — authority_grounding would move away from distributed toward an extraction-grounded reading, and the classification of the administrative seats shifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_kernel_framing_underdetermination, conceptual, 'Two coherent kernel framings produce different commitment-system classifications; the choice is guided by where disputes actually locate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__hybrid_contextual_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__hybrid_contextual_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(woma_tr_t4, woman_female_category__hybrid_contextual_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(woma_tr_t8, woman_female_category__hybrid_contextual_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(woma_tr_t12, woman_female_category__hybrid_contextual_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(woma_tr_t16, woman_female_category__hybrid_contextual_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__hybrid_contextual_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(woma_tr_t24, woman_female_category__hybrid_contextual_reading, theater_ratio, 24, 0.35).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__hybrid_contextual_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(woma_be_t4, woman_female_category__hybrid_contextual_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(woma_be_t8, woman_female_category__hybrid_contextual_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(woma_be_t12, woman_female_category__hybrid_contextual_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(woma_be_t16, woman_female_category__hybrid_contextual_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(woma_be_t20, woman_female_category__hybrid_contextual_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(woma_be_t24, woman_female_category__hybrid_contextual_reading, base_extractiveness, 24, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__hybrid_contextual_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(woma_su_t4, woman_female_category__hybrid_contextual_reading, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(woma_su_t8, woman_female_category__hybrid_contextual_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(woma_su_t12, woman_female_category__hybrid_contextual_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(woma_su_t16, woman_female_category__hybrid_contextual_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(woma_su_t20, woman_female_category__hybrid_contextual_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(woma_su_t24, woman_female_category__hybrid_contextual_reading, suppression_requirement, 24, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__hybrid_contextual_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'who counts as a woman/female'. The label covers three structurally distinct claims with different victim sets, enforcement structures, and epsilon values; per the epsilon-invariance principle they are authored as three files. This file is the hybrid contextual reading (domain-shifting victim set, moderate cross-context epsilon, institutional conflict-minimization beneficiaries). The sex_biology_reading upstream sibling supplies the bio-keyed half of this arrangement's justification; the gender_identity_reading upstream sibling supplies the identity-keyed half. Each monist reading cites the domains where the hybrid already agrees with it as evidence for universalizing its own criterion — the hybrid is the contested middle whose instability propagates outward to both siblings. Family members are mutually linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_female_category__hybrid_contextual_reading, moderate, 0.62).
constraint_indexing:directionality_override(woman_female_category__hybrid_contextual_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
