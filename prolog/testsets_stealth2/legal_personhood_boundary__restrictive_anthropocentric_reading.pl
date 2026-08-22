% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__restrictive_anthropocentric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__restrictive_anthropocentric_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: legal_personhood_boundary__restrictive_anthropocentric_reading
 *   human_readable: Personhood Boundary — Restrictive Anthropocentric Reading (Born Humans Only)
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the legal_personhood_boundary
 *   kernel: the restrictive_anthropocentric_reading, under which legal
 *   personhood attaches to born human beings and to no other class — not to
 *   pre-birth human organisms, not to nonhuman animals or ecosystems, not to
 *   artificial systems. The standing arrangement this story is about is the
 *   birth-threshold, species-bounded personhood regime governing most
 *   contemporary legal systems; epsilon is authored by this reading's own
 *   lights over that arrangement, per the kernel-reading referent rule. Under
 *   those lights the arrangement is protective and autonomy-maximizing: it
 *   confers unconditional standing on every born human, removes any rival
 *   rights-holder from gestation, and governs the excluded classes
 *   instrumentally (anti-cruelty statutes, research ethics, product law)
 *   rather than as rights-bearers. The reading recognizes no victims — that
 *   is not evasion but its core definitional move, and it is why no victims
 *   array is authored here. The two sibling readings are OTHER constraints
 *   with their own stories, their own epsilon values, and discontinuously
 *   different victim sets; they are linked via network.affects_constraints
 *   and decomposed in the dual-formulation note. Claim and metrics are
 *   authored independently: the claim is rope (a genuine coordination
 *   function — a determinate class of rights-bearers — with net beneficiaries
 *   and no identified extraction), while the metrics describe the
 *   arrangement's actual operation, including a rising maintenance burden the
 *   claim does not preempt. KEY AGENTS (by structural relationship): -
 *   pregnant_people: Primary beneficiary (moderate/constrained) — no rival
 *   legal subject arises in gestation; reproductive decisions remain theirs -
 *   born_humans: Primary beneficiary (moderate/constrained) — unconditional,
 *   uncontested personhood from the moment of birth -
 *   reproductive_medicine_providers: Secondary beneficiary (organized/mobile)
 *   — legal clarity that obstetric and termination care involve one legal
 *   subject, not two - constitutional_courts: Agenda setter
 *   (institutional/identity_locked) — administer and defend the boundary;
 *   accrue interpretive authority; absorb the contestation docket -
 *   fetal_rights_movements: Excluded claimant (organized/constrained) —
 *   contest the birth threshold through litigation, legislation, and
 *   amendment campaigns - animal_rights_jurisprudents: Excluded claimant
 *   (moderate/constrained) — press capacity-based recognition for sentient
 *   nonhumans - ai_rights_proponents: Excluded claimant
 *   (moderate/constrained) — propose recognition for behaviorally capable
 *   artificial systems - comparative_jurists: Analytical observer
 *   (analytical/analytical) — maps where the boundary holds, erodes, or has
 *   moved across jurisdictions - the_human_fetus, sentient_nonhuman_animals,
 *   advanced_artificial_systems: non-agent entities retained for completeness
 *   (agent=false) — the classes whose standing the boundary withholds; they
 *   hold no seat and derive no directionality under this reading
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.21).
domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.37).
domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, extractiveness, 0.21).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0.37).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, rope).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Personhood Boundary — Restrictive Anthropocentric Reading (Born Humans Only)").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__restrictive_anthropocentric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, '9e2cdd57-7319-484b-9089-a91bbccb8c82').
narrative_ontology:cs_kernel_codification('9e2cdd57-7319-484b-9089-a91bbccb8c82', formalized).
narrative_ontology:cs_authority_grounding('9e2cdd57-7319-484b-9089-a91bbccb8c82', lineage).
narrative_ontology:cs_interpretation_layer_present('9e2cdd57-7319-484b-9089-a91bbccb8c82').
narrative_ontology:cs_reading_relation('9e2cdd57-7319-484b-9089-a91bbccb8c82', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_reading_relation('9e2cdd57-7319-484b-9089-a91bbccb8c82', legal_personhood_boundary__functional_capacity_reading, forecloses).
narrative_ontology:cs_axiom('9e2cdd57-7319-484b-9089-a91bbccb8c82', foundational, legal_personhood_attaches_at_live_birth).
narrative_ontology:cs_axiom_status(legal_personhood_attaches_at_live_birth, holdable).
narrative_ontology:cs_axiom_grounding('9e2cdd57-7319-484b-9089-a91bbccb8c82', legal_personhood_attaches_at_live_birth, conventional).
narrative_ontology:cs_axiom('9e2cdd57-7319-484b-9089-a91bbccb8c82', foundational, human_species_dignity_grounds_personhood).
narrative_ontology:cs_axiom_status(human_species_dignity_grounds_personhood, holdable).
narrative_ontology:cs_axiom_grounding('9e2cdd57-7319-484b-9089-a91bbccb8c82', human_species_dignity_grounds_personhood, deontological).
narrative_ontology:cs_reference_frame('9e2cdd57-7319-484b-9089-a91bbccb8c82', birth_threshold_species_line_settlement).
narrative_ontology:cs_drift_state('9e2cdd57-7319-484b-9089-a91bbccb8c82', contemporary, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('9e2cdd57-7319-484b-9089-a91bbccb8c82', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_people).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, born_humans).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_medicine_providers).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, birth_threshold_personhood_rule).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_privacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Carry pregnancies and make reproductive decisions under a legal order in which no second rights-holder arises during gestation. Termination, continuation, and prenatal conduct are governed as their decisions alone. What flows to them is exclusive decisional authority over pregnancy; the accommodation of a competing legal subject that a sibling reading would require never arises. Relocation across jurisdictions is possible but costly, and no jurisdiction offers a materially more autonomous settlement than this one affords them.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_people, beneficiary,
    moderate, biographical, constrained, global).

% Hold legal personhood unconditionally from the moment of birth, without tests of capacity, contribution, or conduct. Every contract right, tort protection, criminal-law safeguard, and political entitlement presupposes this status. What flows to them is stable, never-contested standing for life; what they forgo is membership in a legal order that might have recognized wider classes of subjects. Exiting the arrangement would mean exiting law itself, which is unavailable to anyone.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, born_humans, beneficiary,
    moderate, generational, constrained, global).

% Physicians, clinics, and professional bodies providing obstetric and termination care. Because gestation involves one legal subject rather than two, their consent obligations, liability exposure, and criminal exposure are defined by a single patient relationship. Professional organizations are built around this clarity; individual practitioners can change specialty or jurisdiction, though the clarity follows borders imperfectly.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_medicine_providers, beneficiary,
    organized, biographical, mobile, national).

% Administer the personhood boundary: hear petitions to extend or relocate it, reject or occasionally accommodate them, and maintain the doctrinal line across generations of judges. What accrues to the bench is interpretive authority — the boundary is one of the foundations courts are trusted to guard. What they absorb is the contestation docket, political pressure after visible rulings, and the legitimacy risk of holding a line a large minority rejects. The institution's identity is fused with its interpretive role; stepping outside the settled frame is not a move available to it as an institution.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, constitutional_courts, agenda_setter,
    institutional, generational, identity_locked, national).

% Organized movements seeking legal recognition of pre-birth human organisms as persons. They litigate, sponsor legislation and ballot measures, and pursue constitutional amendment. Every avenue runs through institutions that apply the birth threshold; their victories to date have been incremental protections that stop short of personhood. Their realistic paths are persuasion over generations or jurisdictional experiments; abandoning the goal would dissolve the movement's purpose.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, fetal_rights_movements, excluded,
    organized, generational, constrained, national).

% Scholars and litigators arguing that sentient nonhumans should hold legal personhood or person-like standing, using habeas-style actions and capacity evidence. Under the governing criterion their claims arrive as category errors rather than live questions; they operate through academic influence, model jurisdictions, and occasional judicial sympathy. Exit would mean conceding the species line as final.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, animal_rights_jurisprudents, excluded,
    moderate, generational, constrained, global).

% Researchers, ethicists, and advocates proposing legal recognition for artificial systems that display behaviorally measurable capacities. The governing criterion excludes them by origin and birth alike; their advocacy targets future frameworks, corporate policy, and international instruments. Their timeline is long and their institutional foothold thin.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, ai_rights_proponents, excluded,
    moderate, generational, constrained, global).

% Academic and intergovernmental observers mapping how different jurisdictions draw the personhood line — where it holds at birth, where fetal protections approach personhood without crossing it, where animals have gained non-person legal standing, where AI frameworks are emerging. They publish, advise, and testify; their seat carries no stake in which reading wins.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, comparative_jurists, observer,
    analytical, generational, analytical, global).

% A pre-birth human organism. Under the governing criterion it is not a legal subject; its interests reach the law only through others — pregnancy-crime statutes, wrongful-death conventions in some jurisdictions, medical duty-of-care norms — without conferring personhood. Retained in the story for completeness; it holds no seat and collects nothing under this reading.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, the_human_fetus, excluded,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__restrictive_anthropocentric_reading, the_human_fetus).

% Nonhuman animals with demonstrable sentience — the exact profile a capacity-based sibling reading would admit. Under the governing criterion, species membership bars personhood regardless of measured capacity; they are governed instrumentally through anti-cruelty statutes, welfare regulations, and research rules. Retained for completeness; no seat, no collection.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, sentient_nonhuman_animals, excluded,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__restrictive_anthropocentric_reading, sentient_nonhuman_animals).

% Artificial systems exhibiting behaviorally sophisticated capacities. Under the governing criterion, non-birth and non-human origin bar personhood categorically; they are governed as property, products, and infrastructure. Retained for completeness; no seat, no collection.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, advanced_artificial_systems, excluded,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__restrictive_anthropocentric_reading, advanced_artificial_systems).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__restrictive_anthropocentric_reading, diffuse).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__restrictive_anthropocentric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies the determinate class of rights-bearers that every other legal institution presupposes: contracts require persons who can promise, property requires owners, criminal law requires responsible subjects, representation requires constituents. The birth-plus-species rule settles that class by bright line, eliminating case-by-case adjudication of marginal candidates.
% TRANSFER_FUNCTION: Moves unconditional legal standing and protection to every born human at birth; moves exclusive reproductive decision-authority to pregnant people by leaving no rival claimant; withholds standing, and the protections that ride on it, from pre-birth human organisms, nonhuman animals, and artificial systems, whose treatment is routed instead through instrumental regulation controlled by third parties.
% ABSENT_VOICES: The advocates of the excluded classes — fetal-rights movements, animal-rights jurists, AI-rights proponents — would object that the conversation's membership rule pre-decides the very question they raise: they appear only as petitioners before bodies empowered to refuse them by restating the rule. The excluded entities themselves cannot appear at all; their interests enter only as translated by sympathetic third parties. Both groups sit outside the room where the criterion is maintained.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, every legal relation built on it would destabilize at once: who may sue and be sued, who inherits, who is criminally punishable, whether pregnancy houses one legal subject or two, whether animals and AI systems are property or peers. Contract, tort, criminal law, and democratic representation would all re-derive from scratch; the rearrangement would be total and fought over, not smooth.
% FOUNDING_PROBLEM: Law must know who counts as a rights-bearing subject before it can operate at all. After centuries of contest over which humans counted — enslaved people, women, the propertyless — the settlement fixed the class at born humans: a bright line that ended intra-species personhood disputes, made rights administration tractable, and pushed every further boundary question (fetus, animal, machine) outside the category rather than inside it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: legal-historical and comparative-law scholarship documents the permanence of the determinate-rights-bearer problem across jurisdictions; and, decisively, the sibling readings themselves corroborate it — the developmental and functional readings each propose relocating the criterion, not abolishing the need for one, so no serious participant in the dispute denies that law requires a personhood line. No corroboration exists for the claim that the specific birth-plus-species placement is uniquely correct; that placement is attested only by this reading's adherents and the institutions that administer it.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__restrictive_anthropocentric_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__restrictive_anthropocentric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legal_personhood_boundary__restrictive_anthropocentric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.21, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).
:- end_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.21 at interval end) because the referent is the standing arrangement assessed by this reading's own lights: the boundary confers standing rather than taking it, and the classes on the far side of the line are, by the reading's definition, not subjects from whom anything could be extracted. Suppression (0.37) is a raw, unscaled structural property: it measures the interpretive and institutional effort needed to hold the line against sustained challenge — refused petitions, declined amendment campaigns, defeated ballot measures — not coercion of governed subjects, and it is NOT scaled by power or scope (only extractiveness is scaled, by directionality and scope, in the engine's computation). Theater is minimal (0.12): the boundary is load-bearing in every contract, prosecution, and inheritance; almost nothing about it is performed. Accessibility collapse is low (0.30): understanding the reading does not close the alternatives — both sibling readings remain fully available positions, which is precisely why resistance is substantial (0.50): this kernel hosts one of the longest-running live contests in legal philosophy. The measurement series run on one shared six-point grid so every tracked metric is authored at every examined time point; the gentle extractiveness rise tracks multiplying edge cases (earlier viability, sentience findings, capable AI) that make the line costlier to hold by the reading's own accounting. Receipt surface: gain_flow='diffuse' is an affirmative claim — each named seat was checked and none captures the arrangement's gains, which distribute across the included classes; fixing_cost='prohibitive' reflects that wholesale revision would reopen every settled doctrine that presupposes the person class. The diffuse-plus-prohibitive cell is piton-flavored by the prototype's cell semantics, but the fit fails on the piton test proper: function is fully intact, theater is minimal, and maintenance is active rather than inertial — the divergence is authored honestly as data, not reconciled.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the engine should see it. From constitutional_courts the boundary is settled doctrine under stewardship — a source of authority and of docket burden at once. From pregnant_people it is the foundation of reproductive autonomy: the difference between a private decision and a negotiation with a second rights-holder. From the excluded claimant seats the same structure is a closed door: their petitions are not denied on the merits but ruled category errors by the membership rule itself. The three non-agent classes compute no seat at all under this reading — which is the reading's central move, and the precise point where the sibling stories diverge: the developmental reading's story will seat the fetus as a protected beneficiary and compute sharply higher extraction from the pregnant person's side; the functional reading's story will seat sentient nonhumans and capable AI as beneficiaries and compute high extraction from the species line. Same kernel, three constraints, three epsilon values — the cross-reading divergence is the corpus-level measurement this family exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: pregnant_people, born_humans, and reproductive_medicine_providers sit near the beneficiary end (low d, damped or inverted effective extraction). One override is authored: constitutional_courts (institutional) to d=0.30 — the derivation would leave the agenda_setter seat at a neutral fallback, but structurally the courts are near-beneficiary: they accrue interpretive authority from administering the boundary while absorbing its maintenance costs, a mildly subsidized position. The excluded claimant seats (fetal_rights_movements, animal_rights_jurisprudents, ai_rights_proponents) carry high structural target-position through their excluded role and constrained exits, but are deliberately NOT declared victims: under this reading's own lights they are disappointed petitioners, not extracted-upon subjects, and importing a victim declaration would smuggle a sibling reading's ontology into this story and break epsilon-invariance. The three non-agent classes are flagged agent=false so they feed no directionality arithmetic at all — collecting nothing and paying nothing is the reading's verdict about them, and the structural data should say so.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim guards against two opposite mislabels. Against snare: the arrangement's exclusionary effects do not make it extraction, because exclusion of entities the reading defines as non-subjects leaves no one extracted from — a snare coding would require victims this reading denies exist, and authoring them here would falsify the reading rather than measure it. Against complacent rope certification forever: the rising suppression_requirement series is authored so the engine can see maintenance hardening; if interpretive gatekeeping converts to overt coercion, the computed type should migrate and this story's claim should lose. Mandatrophy is clean: the founding problem — law's need for a determinate class of rights-bearers — is live and corroborated from outside the benefiting parties (including by the sibling readings themselves, each of which proposes relocating the criterion rather than abolishing the need for one); mandate and function coincide, so no zombie-mandate flag is expected from the status-times-verdict mismatch consumer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the restrictive_anthropocentric_reading of the legal_personhood_boundary kernel; the sibling readings (developmental_potentiality_reading, functional_capacity_reading) instantiate different constraints over the same kernel — which criterion governs is the live contest, and each reading authors its own epsilon, beneficiaries, and victims.',
    'Generate the sibling stories separately and compare per-seat classifications across the family; the disagreement is located in the personhood criterion (birth-plus-species versus conception versus demonstrated capacity), not in any observable of a single constraint.',
    'If a sibling reading displaces this one in a jurisdiction, the victim set changes discontinuously — fetuses enter as protected persons under the developmental reading, sentient nonhumans and capable AI enter under the functional reading — and this story''s low reading-indexed epsilon ceases to describe the governing arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame omega: one reading of a three-reading kernel; sibling readings are other constraints, not hedges inside this one.').

omega_variable(
    cognitive_capacity_clause_status,
    'Within this reading, is ''cognitive capacity'' a strict eligibility test that would exclude born humans lacking it (anencephalic infants, permanent vegetative states), or a contrastive marker that leaves personhood presumptive for all born humans?',
    'Examine how proponents of the reading treat marginal human cases in scholarship and litigation: strict-test proponents concede exclusion of some born humans; presumptive proponents reserve the capacity language for the species boundary alone.',
    'Strict application shrinks the protected class, creates internal victims, and raises epsilon well above the reading-indexed value authored here; presumptive application preserves the low-extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_capacity_clause_status, conceptual, 'Whether the capacity clause is an eligibility filter or a species-boundary marker.').

omega_variable(
    welfare_externalization_blindness,
    'The reading fixes the moral status of excluded classes at zero by definitional fiat; if late-gestation fetuses, sentient nonhumans, or future AI systems have welfare stakes, the arrangement externalizes costs its own evaluative lights cannot register — is the low epsilon an artifact of ontological closure rather than a measured absence of harm?',
    'Independent welfare-science assessment of excluded classes (fetal sentience thresholds, animal cognition, AI welfare markers) conducted without adopting the reading''s ontology, then compared against the reading-indexed epsilon.',
    'Confirmation would split epsilon into a reading-indexed value (low, as authored) and a welfare-grounded value (substantially higher) — the divergence itself becomes the finding, and the arrangement acquires tangled-rope structure from seats outside the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_externalization_blindness, empirical, 'Whether definitional closure hides real externalized welfare costs.').

omega_variable(
    maintenance_burden_trajectory,
    'Will the interpretive maintenance burden keep rising with contestation intensity (advancing neonatology, sentience science, AI capability) until holding the boundary requires overt coercive exclusion rather than interpretive gatekeeping?',
    'Track the suppression_requirement series and the composition of personhood-related dockets; a continued rise past roughly 0.5 accompanied by doctrinal retreats would signal conversion pressure.',
    'Continued rise pressures reclassification toward enforced-extraction structure; a plateau confirms stable coordination-dominant operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintenance_burden_trajectory, empirical, 'Whether enforcement-ratchet dynamics are converting interpretive maintenance into coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lpb_restrictive_tr_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(lpb_restrictive_tr_t0, observed).
narrative_ontology:measurement(lpb_restrictive_tr_t6, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 6, 0.09).
narrative_ontology:measurement_basis(lpb_restrictive_tr_t6, observed).
narrative_ontology:measurement(lpb_restrictive_tr_t12, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 12, 0.09).
narrative_ontology:measurement_basis(lpb_restrictive_tr_t12, observed).
narrative_ontology:measurement(lpb_restrictive_tr_t18, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement_basis(lpb_restrictive_tr_t18, observed).
narrative_ontology:measurement(lpb_restrictive_tr_t24, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement_basis(lpb_restrictive_tr_t24, observed).
narrative_ontology:measurement(lpb_restrictive_tr_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(lpb_restrictive_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(lpb_restrictive_be_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement_basis(lpb_restrictive_be_t0, observed).
narrative_ontology:measurement(lpb_restrictive_be_t6, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 6, 0.17).
narrative_ontology:measurement_basis(lpb_restrictive_be_t6, observed).
narrative_ontology:measurement(lpb_restrictive_be_t12, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 12, 0.18).
narrative_ontology:measurement_basis(lpb_restrictive_be_t12, observed).
narrative_ontology:measurement(lpb_restrictive_be_t18, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 18, 0.19).
narrative_ontology:measurement_basis(lpb_restrictive_be_t18, observed).
narrative_ontology:measurement(lpb_restrictive_be_t24, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 24, 0.2).
narrative_ontology:measurement_basis(lpb_restrictive_be_t24, observed).
narrative_ontology:measurement(lpb_restrictive_be_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 30, 0.21).
narrative_ontology:measurement_basis(lpb_restrictive_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(lpb_restrictive_su_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(lpb_restrictive_su_t0, observed).
narrative_ontology:measurement(lpb_restrictive_su_t6, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 6, 0.25).
narrative_ontology:measurement_basis(lpb_restrictive_su_t6, observed).
narrative_ontology:measurement(lpb_restrictive_su_t12, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 12, 0.28).
narrative_ontology:measurement_basis(lpb_restrictive_su_t12, observed).
narrative_ontology:measurement(lpb_restrictive_su_t18, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 18, 0.31).
narrative_ontology:measurement_basis(lpb_restrictive_su_t18, observed).
narrative_ontology:measurement(lpb_restrictive_su_t24, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 24, 0.34).
narrative_ontology:measurement_basis(lpb_restrictive_su_t24, observed).
narrative_ontology:measurement(lpb_restrictive_su_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 30, 0.37).
narrative_ontology:measurement_basis(lpb_restrictive_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, developmental_potentiality_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, functional_capacity_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the legal_personhood_boundary kernel, per the epsilon-invariance principle: the colloquial label 'who is a person' covers three structurally distinct arrangements — this story (restrictive_anthropocentric_reading: birth-plus-species criterion, no victims by its own lights, low reading-indexed epsilon), developmental_potentiality_reading (conception criterion; fetuses enter the protected class and pregnancy becomes a two-subject relation; high extraction computed from the pregnant person's side), and functional_capacity_reading (capacity criterion; sentient nonhumans and capable AI enter; high extraction computed from the species line). The colloquial label was the confusion; the criterion is the constraint. This story is the upstream incumbent: it governs practice in most jurisdictions and thereby sets the operating environment — docket access, doctrinal precedent, legitimacy conditions — within which both sibling readings litigate; each sibling story links back here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_personhood_boundary__restrictive_anthropocentric_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
