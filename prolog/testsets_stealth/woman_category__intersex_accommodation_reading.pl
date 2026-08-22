% ============================================================================
% CONSTRAINT STORY: woman_category__intersex_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__intersex_accommodation_reading, []).

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
 *   constraint_id: woman_category__intersex_accommodation_reading
 *   human_readable: Woman-Category Membership: Intersex-Accommodation Reading
 *   domain: political philosophy/law/social policy/bioethics
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested category-membership
 *   kernel 'woman': the intersex-accommodation reading, under which category
 *   membership follows biology understood as a spectrum - typical female
 *   biology qualifies, and intersex variations that do not fit the male
 *   category also qualify. The arrangement under measurement is that
 *   criterion in operation across the institutions that gate access on it:
 *   civil registration and documents, single-sex spaces and services, and
 *   elite sport, where the criterion binds hardest because every boundary
 *   ruling redistributes places, records, and prize money. The criterion
 *   solves a real classification problem - the strict binary leaves people
 *   with variations of sex characteristics unclassifiable or miscategorized -
 *   but it does not abolish the boundary; it redraws it, and the redrawn line
 *   is enforced by testing, panels, and litigation at the sports site, where
 *   costs fall on both sides: boundary-case athletes face continuing
 *   adjudication of their own bodies, and female-typical athletes bear the
 *   competitive consequences of admitted boundary cases. KEY AGENTS (by
 *   structural relationship): - intersex_dsd_individuals: primary beneficiary
 *   (moderate/trapped) - gain unconditional membership in ordinary
 *   institutions - typical_female_category_members: beneficiary with
 *   secondary cost-bearing position (organized/constrained) - stable
 *   biologically anchored category in civil life, competitive exposure in
 *   sport - sports_governing_bodies: agenda_setter with secondary beneficiary
 *   position (institutional/arbitrage) - writes and administers the fitness
 *   tests, absorbs the controversy - dsd_boundary_athletes: primary payer at
 *   the enforcement edge (powerless/trapped) - testing, panels,
 *   medicate-or-leave choices; the Semenya class -
 *   female_typical_elite_athletes: payer at the sports site
 *   (organized/constrained) - displaced outcomes when boundary cases are
 *   admitted - trans_women_excluded_by_biology_test: excluded voice
 *   (moderate/trapped) - the criterion assigns them outside the category and
 *   hears no identity claim - sex_binary_policy_advocates: excluded voice
 *   (organized/mobile) - contest every inclusion from legislatures and media
 *   - human_rights_courts: analytical observer with coercive remedies
 *   (analytical/analytical) - review eligibility rulings against
 *   anti-discrimination guarantees. Family note: the colloquial label 'who
 *   counts as a woman' decomposes into three structurally distinct
 *   constraints (this reading plus the sex-biology and gender-identity
 *   siblings), each with its own epsilon, victim set, and classification;
 *   they are linked via network.affects_constraints, and the
 *   domain-concentration omega documents the further civil/sport
 *   decomposition this story may itself require. Claim/metric independence:
 *   the claimed type (tangled_rope) states what I believe structurally true
 *   of this arrangement; the metrics state what I believe descriptively true
 *   of its operation; neither was tuned to the other or to a predicted engine
 *   output.
 *
 * KEY AGENTS:
 *   - intersex_dsd_individuals: primary beneficiary (moderate/trapped) - recognized without identity argument or bodily alteration in ordinary institutions
 *   - typical_female_category_members: beneficiary, secondarily cost-bearing (organized/constrained) - keep a biology-anchored category; carry sports-site competitive costs
 *   - sports_governing_bodies: agenda_setter, secondarily beneficiary (institutional/arbitrage) - operate the eligibility machinery, absorb litigation and controversy
 *   - dsd_boundary_athletes: primary payer at the enforcement edge (powerless/trapped) - tested, adjudicated, faced with medicate-or-leave; the Semenya class
 *   - female_typical_elite_athletes: payer at the sports site (organized/constrained) - lose places and records when boundary cases compete
 *   - trans_women_excluded_by_biology_test: excluded (moderate/trapped) - assigned outside the category; the criterion hears no identity claim
 *   - sex_binary_policy_advocates: excluded (organized/mobile) - contest the inclusions from outside the criterion's own panels
 *   - human_rights_courts: analytical observer (analytical/analytical) - review the criterion's operation against anti-discrimination guarantees
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.42).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.5).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Woman-Category Membership: Intersex-Accommodation Reading").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political philosophy/law/social policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__intersex_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, '8917cec0-d654-4984-933d-6a830eb39d14').
narrative_ontology:cs_kernel_codification('8917cec0-d654-4984-933d-6a830eb39d14', distributed).
narrative_ontology:cs_authority_grounding('8917cec0-d654-4984-933d-6a830eb39d14', distributed).
narrative_ontology:cs_reading_relation('8917cec0-d654-4984-933d-6a830eb39d14', woman_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('8917cec0-d654-4984-933d-6a830eb39d14', woman_category__gender_identity_reading, forecloses).
narrative_ontology:cs_axiom('8917cec0-d654-4984-933d-6a830eb39d14', foundational, biological_sex_is_nonbinary_spectrum).
narrative_ontology:cs_axiom_status(biological_sex_is_nonbinary_spectrum, holdable).
narrative_ontology:cs_axiom_grounding('8917cec0-d654-4984-933d-6a830eb39d14', biological_sex_is_nonbinary_spectrum, empirically_contingent).
narrative_ontology:cs_axiom('8917cec0-d654-4984-933d-6a830eb39d14', foundational, category_membership_tracks_biology_not_identity).
narrative_ontology:cs_axiom_status(category_membership_tracks_biology_not_identity, holdable).
narrative_ontology:cs_axiom_grounding('8917cec0-d654-4984-933d-6a830eb39d14', category_membership_tracks_biology_not_identity, instrumental).
narrative_ontology:cs_axiom('8917cec0-d654-4984-933d-6a830eb39d14', secondary, intersex_variations_admitted_where_male_fit_fails).
narrative_ontology:cs_axiom_status(intersex_variations_admitted_where_male_fit_fails, holdable).
narrative_ontology:cs_axiom_grounding('8917cec0-d654-4984-933d-6a830eb39d14', intersex_variations_admitted_where_male_fit_fails, conventional).
narrative_ontology:cs_reference_frame('8917cec0-d654-4984-933d-6a830eb39d14', biology_spectrum_inclusive_membership).
narrative_ontology:cs_drift_state('8917cec0-d654-4984-933d-6a830eb39d14', contemporary_post_semenya_litigation, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('8917cec0-d654-4984-933d-6a830eb39d14', '').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, intersex_dsd_individuals).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, typical_female_category_members).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, dsd_boundary_athletes).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, female_typical_elite_athletes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, sports_governing_bodies).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, typical_female_category_members).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, biology_spectrum_model_of_sex).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, intersex_inclusion_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% People born with variations of sex characteristics that do not match typical definitions of male or female bodies. Under this criterion they are members of the category their biology predominantly occupies, without arguing identity or altering their bodies. What flows to them is recognition in documents, spaces, and most institutions as of right; what is asked of them is biological assessment where their variation sits close to the line the criterion still draws. No one exits a classification regime; the live question is only which side of a line they are placed on.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_dsd_individuals, beneficiary,
    moderate, generational, trapped, global).

% People with typical female biology, the large majority of category members. They receive a stable, biologically anchored category that does not depend on accepting self-declared identity, and they carry almost no cost of the criterion in ordinary civil life. In elite sport they additionally carry the competitive consequences of admitted boundary cases, which is why they hold a second, cost-bearing position. Their options are participating under the criterion as written or organizing to move the line.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, typical_female_category_members, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(woman_category__intersex_accommodation_reading, typical_female_category_members, payer).

% Federations and eligibility panels that operate the criterion where it binds hardest. They write and revise the fitness tests, commission the medical evidence, and issue the rulings that decide who races. Maintaining any criterion exposes them to litigation and controversy from every direction while conferring the authority of being the body that decides. They can rewrite the test between seasons; they cannot stop being the body that must have a test, and they pocket none of the competitive goods the rulings redistribute.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(woman_category__intersex_accommodation_reading, sports_governing_bodies, beneficiary).

% Elite athletes whose sex development places them near the line the criterion draws - close enough to the male-category fit question that their eligibility is contested. They face testing, panel proceedings, and in some regimes the choice between medicating a healthy body or leaving the category. Their competitive window is short, no parallel women's circuit exists to transfer to, and racing in the male category ends the career they trained for. Caster Semenya is the paradigm case.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, dsd_boundary_athletes, payer,
    powerless, biographical, trapped, global).

% Elite athletes with typical female biology competing in the same events. When boundary-case athletes are admitted they lose places, records, and prize money to competitors whose physiology they regard as differently prepared; when the line is tightened they regain those outcomes. They organize through athlete associations and can lobby federations, but they cannot opt out of the single category they compete in.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, female_typical_elite_athletes, payer,
    organized, biographical, constrained, global).

% Trans women whose biology fits neither prong of the criterion - neither typical female biology nor an intersex variation. The criterion assigns them outside the category regardless of identity or transition. They would contest the rule's refusal to hear identity claims, but its adjudication takes biological evidence only; their objection lives in the political arena around the rule, not inside it.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, trans_women_excluded_by_biology_test, excluded,
    moderate, biographical, trapped, global).

% Advocates and lawmakers who hold that the category is and should be strictly binary. They contest every inclusion this criterion grants and campaign to restore chromosome-or-anatomy tests in documents and sport. They operate in legislatures and media rather than in the criterion's own panels, and they can move between jurisdictions freely.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sex_binary_policy_advocates, excluded,
    organized, biographical, mobile, national).

% Regional and international tribunals that review eligibility rulings and classification regimes against anti-discrimination guarantees. They do not run the criterion; they hear the cases its operation generates and can compel revision, as the European Court of Human Rights did in the Semenya litigation. Their seat is analytical, with coercive remedies attached to its findings.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, human_rights_courts, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__intersex_accommodation_reading, diffuse).
narrative_ontology:fixing_cost_class(woman_category__intersex_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, biology-anchored criterion for membership in the category 'woman' that classifies the overwhelming majority without controversy and resolves intersex boundary cases by asking whether the variation fits the male category rather than by chromosomes alone or identity alone - coordinating access decisions across registration, documents, spaces, medicine, and sport.
% TRANSFER_FUNCTION: Moves recognition and access to the category and its attached goods toward people with intersex variations who strict-binary rules exclude or condition. At the elite-sport site it moves competitive outcomes - places, records, prize money - from female-typical athletes toward admitted boundary-case athletes, and it moves boundary athletes' time and bodily autonomy into testing and panel proceedings.
% ABSENT_VOICES: Trans women whose biology fits neither prong would object that the criterion excludes them and refuses to hear identity claims; strict-binary advocates would object to every inclusion the criterion grants; both stand outside the rule's own adjudication, which takes biological evidence only. The unanimity of the criterion's panels is partly an artifact of who is permitted to address them.
% DISAPPEARANCE_RATIONALE: If the criterion vanished overnight, every gated decision it settles - registrations, space access, sport eligibility - would lose its operating rule, and the two sibling criteria would contend to replace it: chromosomal-binary forces would reinstate testing regimes that re-exclude the intersex population, identity-based forces would admit populations this criterion excludes, and the Semenya-line litigation would restart from zero under whichever rule won. The arrangements of every named stakeholder depend on which criterion holds.
% FOUNDING_PROBLEM: Binary male/female classification produced unclassifiable or miscategorized people wherever biology varied from the typical case: infants with ambiguous genitalia subjected to nonconsensual normalization surgery, adults whose documents mismatched their presentation and lived sex, athletes barred from the category matching their legal sex and upbringing. The intersex-accommodation reading was built to solve this - classify by biology as a spectrum so boundary people land inside the category their biology predominantly occupies, without surgery, identity litigation, or chromosome tests.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties by Court of Arbitration for Sport award records in the Semenya litigation (documenting the enforcement harms the criterion responds to), UN Independent Expert and Special Rapporteur reports on the human rights of intersex persons, and pediatric-endocrinology consensus statements documenting the harms of binary enforcement and normalization practices. None of these sources is a beneficiary of the criterion. Counter-attestation exists: sports federations attest that the remaining live problem at their site is competitive fairness rather than classification harm, so the problem's content is disputed even where its existence is not.
narrative_ontology:disappearance_verdict(woman_category__intersex_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__intersex_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__intersex_accommodation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_category__intersex_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__intersex_accommodation_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__intersex_accommodation_reading_tests).
:- end_tests(woman_category__intersex_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.42 is authored for the arrangement as a whole: routine civil operation of the criterion costs its subjects almost nothing (classification of the overwhelming majority is uncontested and uncoerced), while the binding edge - elite sport - concentrates real extraction, since each boundary ruling transfers competitive goods and subjects athletes' bodies to compulsory assessment. The scalar necessarily blends a near-zero-cost civil regime with a heavily extractive sports edge; the domain_concentration_decomposition omega flags that per-site values would run roughly 0.1 and 0.8 respectively, and that decomposition is the honest next step if the engine's per-seat computation diverges. Suppression 0.50 reflects machinery that must exist for the criterion to hold anywhere it is contested - eligibility testing, panel procedure, documentation regimes - without approaching the coercive density of arrangements that depend on suppressing exits for their core function. Theater_ratio 0.32: the adjudicative function is real (genuine boundary cases exist and are genuinely decided), but a growing share of activity is performative - fairness rhetoric, publicized rulings, institutional statements - proxy work that accompanies rather than constitutes the function. Accessibility_collapse 0.50: within the criterion's own framework the sibling criteria are unavailable (an agent subject to this rule cannot elect the identity-based or chromosomal test), but alternative readings remain live in the surrounding polity and some jurisdictions already implement neighboring rules, so alternatives are suppressed locally, not globally. Resistance 0.55: the criterion is attacked from two flanks simultaneously - exclusion objections from trans advocates and inclusion objections from binary advocates - plus litigation from boundary athletes, giving it unusually multi-directional opposition for a rule most people are never consciously governed by. Temporal design: one shared grid (t = 0, 6, 12, 18, 24, 30; roughly 1990 to 2020) carries all three tracked metrics at every point, per the alignment rule. Base_extractiveness rises monotonically with the criterion's institutional uptake - as the accommodation approach spreads from marginal proposal to operating rule, its extractive edge activates and concentrates at the sports site. Theater_ratio rises in step as compliance and reputation layers accumulate atop the adjudicative core. Suppression_requirement traces a hump, not a trend: enforcement capacity was built up through the hyperandrogenism and DSD regulation era (peak near t=18), then partially eroded under litigation pressure and documentation liberalization (falling to 0.50 by t=30). The hump is documented rather than smoothed because the enforcement build-and-partial-decay is the story's real dynamic; whether the decline is durable is carried by the suppression_decline_durability omega. Base_properties values equal the t=30 endpoints of their series.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience the same criterion as four different arrangements. From the sports_governing_bodies seat the criterion is a governance instrument it writes, revises, and defends - coordination it administers, with controversy as an operating cost. From the dsd_boundary_athletes seat the same criterion is a permanent tribunal convened over their own bodies, with no exit that preserves their careers. From the female_typical_elite_athletes seat it is a rule that decides who they must race, experienced as cost-bearing at the sports site while remaining cost-free in every other domain of their lives. From the intersex_dsd_individuals seat (outside elite sport) it is straightforward recognition - the first membership rule that does not require them to argue their way in. The engine computes these divergent per-seat classifications from the structural data; the divergence between the agenda-setter's coordinative experience and the trapped payers' extractive experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Structural declarations drive the derivation. intersex_dsd_individuals are declared beneficiaries with trapped exit: the criterion's principal good flows to them, and their d sits near the beneficiary end - the trapped atom correctly tempers the subsidy, since no one exits a classification regime that allocates them. typical_female_category_members hold beneficiary with a secondary payer role: large ordinary-domain benefit, real sports-site cost, landing them near symmetric. dsd_boundary_athletes and female_typical_elite_athletes are declared payers with trapped and constrained exit respectively: both sit near the target end, the boundary athletes closer because their exposure is compulsory and their exit destroys their careers. sports_governing_bodies are agenda_setter with a secondary beneficiary role; left to the beneficiary signal alone the derivation would push their d artificially low, but they absorb the enforcement costs and capture no share of the redistributed competitive goods, so a directionality_override sets the institutional atom to 0.35 (near-symmetric administrator). The override is safe at the atom level because no other stakeholder holds the institutional atom - human_rights_courts carry the analytical atom. trans_women_excluded_by_biology_test and sex_binary_policy_advocates are excluded voices: the former are structurally targeted by the criterion's refusal of identity claims (high d despite not being a declared victim group - the exclusion is the cost), the latter are mobile opponents whose d is attenuated by jurisdictional arbitrage. Suppression is authored as a raw structural property and is deliberately unscaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - binary classification rendering people with variations of sex characteristics unclassifiable, surgically normalized without consent, or barred from the category matching their lives - is still live: Semenya-line litigation continues, documentation mismatches persist, and the criterion's own boundary generates fresh cases. founding_problem_status is therefore live and mandatrophy is not resolved; the mismatch consumer reading (status=live x verdict=world_rearranges) raises no zombie flag. The tangled_rope claim is what prevents mislabeling in both directions: a snare reading (pressed by the criterion's binary opponents, who see only the enforced line and the tested bodies) would erase the genuine coordination - daily, load-bearing classification of documents, medicine, and spaces that functions without coercion for virtually everyone governed; a rope reading (pressed by the criterion's supporters, who see only the inclusion) would erase the asymmetric extraction at the sports edge, where a small, powerless, trapped population bears compulsory adjudication and a second population bears displaced outcomes through the same structure. The piton alternative fails on the facts: the coordination function has not atrophied (theater_ratio 0.32 is real-but-minority performance, not the dominant mode), the rule performs its classification work continuously, and fixing_cost is prohibitive not because the function is dead but because every redrawing of the line re-litigates the same multi-front controversy - cost-asymmetry born of live contention, not institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading (intersex_accommodation_reading) of the kernel woman_category; what would each sibling reading change structurally if instantiated in place of this one?',
    'Cross-reading comparison of victim sets and epsilon over the same category-gating arrangement: the sex_biology_reading shifts the victim set to all intersex people plus trans people (chromosome/anatomy tests exclude them), while the gender_identity_reading shifts it toward female-typical athletes and space users bearing self-ID costs. The disagreement is located in the membership criterion itself - biology-spectrum-fit versus chromosomal binary versus internal identity.',
    'Per-seat classifications computed from this story''s structural data describe only this reading''s arrangement; the same gating infrastructure classified under a sibling reading yields a different victim set, different epsilon, and potentially a different type. Cross-reading divergence is the meta-analytic object and must not be reconciled inside this story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one of three readings of the woman_category kernel; sibling readings instantiate different constraints.').

omega_variable(
    domain_concentration_decomposition,
    'Extractiveness is near-zero in ordinary civil administration (documents, spaces, medicine) but high at the elite-sport site where every boundary ruling redistributes competitive outcomes - is this one constraint or a constraint family requiring decomposition?',
    'Per-site measurement of effective extraction: if the sports-site computation diverges beyond threshold from the civil-administration computation, split into woman_category__intersex_accommodation_civil and woman_category__intersex_accommodation_sport, linked via network edges, per the epsilon-invariance decomposition rule.',
    'The single authored epsilon of 0.42 blends a near-zero-cost civil regime with a heavily extractive sports edge; decomposition would likely yield a rope in civil domains and a strongly extractive tangled_rope (or worse) at the sports site, dating type transitions differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_concentration_decomposition, empirical, 'Whether the domain-concentrated extraction profile satisfies epsilon-invariance within one story or mandates a two-file split.').

omega_variable(
    male_category_fit_under_specification,
    'Where exactly does the criterion''s own boundary sit - what makes an intersex variation ''fit the male category'' (chromosomes, testosterone range, anatomy, developed performance capacity)?',
    'Accumulated eligibility case law and federation test revisions building an adjudicated fit standard; each ruling (Semenya-line cases) pins the line more precisely.',
    'Moving the line reallocates the victim set between dsd_boundary_athletes (more exposed as the line tightens) and female_typical_elite_athletes (more exposed as it loosens); epsilon swings materially with each move while the rule''s label stays constant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(male_category_fit_under_specification, conceptual, 'The reading''s internal boundary is under-specified; its placement drives the victim-set distribution.').

omega_variable(
    fairness_tradeoff_preference_residual,
    'Is admitting boundary-case athletes into the women''s category an unfair cost imposed on female-typical athletes, or a legitimate inclusion trade-off?',
    'Not fully resolvable by data: physiological advantage measurements bound the empirical input, but the weighting of competitive fairness against inclusion is a values dispute held by different parties.',
    'If the sports-site cost is counted as harm, epsilon rises and the tangled_rope reading strengthens; if it is counted as a fair price of open competition, epsilon falls toward rope. The classification is preference-sensitive at exactly this point.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fairness_tradeoff_preference_residual, preference, 'Preference residue in the sports-site extraction assessment.').

omega_variable(
    suppression_decline_durability,
    'Is the post-peak easing of enforcement (litigation losses by federations, spread of non-binary documentation, softened testing regimes) durable enforcement decay, or a trough in an enforcement ratchet that rebuilds?',
    'Track federation rule revisions, new testing proposals, and documentation-policy reversals over the following decade; renewed ratchet behavior would show as rising suppression_requirement after the trough.',
    'Durable decay supports drift toward rope; a rebuilt ratchet supports persistent tangled_rope with intensifying suppression. The current flat-to-falling tail of the suppression series is the datum at risk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_decline_durability, empirical, 'Whether the observed enforcement decline is structural or cyclical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wcat_intersex_acc_tr_t0, woman_category__intersex_accommodation_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(wcat_intersex_acc_tr_t0, observed).
narrative_ontology:measurement(wcat_intersex_acc_tr_t6, woman_category__intersex_accommodation_reading, theater_ratio, 6, 0.17).
narrative_ontology:measurement_basis(wcat_intersex_acc_tr_t6, observed).
narrative_ontology:measurement(wcat_intersex_acc_tr_t12, woman_category__intersex_accommodation_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement_basis(wcat_intersex_acc_tr_t12, observed).
narrative_ontology:measurement(wcat_intersex_acc_tr_t18, woman_category__intersex_accommodation_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement_basis(wcat_intersex_acc_tr_t18, observed).
narrative_ontology:measurement(wcat_intersex_acc_tr_t24, woman_category__intersex_accommodation_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement_basis(wcat_intersex_acc_tr_t24, observed).
narrative_ontology:measurement(wcat_intersex_acc_tr_t30, woman_category__intersex_accommodation_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(wcat_intersex_acc_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(wcat_intersex_acc_be_t0, woman_category__intersex_accommodation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(wcat_intersex_acc_be_t0, observed).
narrative_ontology:measurement(wcat_intersex_acc_be_t6, woman_category__intersex_accommodation_reading, base_extractiveness, 6, 0.24).
narrative_ontology:measurement_basis(wcat_intersex_acc_be_t6, observed).
narrative_ontology:measurement(wcat_intersex_acc_be_t12, woman_category__intersex_accommodation_reading, base_extractiveness, 12, 0.31).
narrative_ontology:measurement_basis(wcat_intersex_acc_be_t12, observed).
narrative_ontology:measurement(wcat_intersex_acc_be_t18, woman_category__intersex_accommodation_reading, base_extractiveness, 18, 0.36).
narrative_ontology:measurement_basis(wcat_intersex_acc_be_t18, observed).
narrative_ontology:measurement(wcat_intersex_acc_be_t24, woman_category__intersex_accommodation_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement_basis(wcat_intersex_acc_be_t24, observed).
narrative_ontology:measurement(wcat_intersex_acc_be_t30, woman_category__intersex_accommodation_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(wcat_intersex_acc_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(wcat_intersex_acc_su_t0, woman_category__intersex_accommodation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(wcat_intersex_acc_su_t0, observed).
narrative_ontology:measurement(wcat_intersex_acc_su_t6, woman_category__intersex_accommodation_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement_basis(wcat_intersex_acc_su_t6, observed).
narrative_ontology:measurement(wcat_intersex_acc_su_t12, woman_category__intersex_accommodation_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement_basis(wcat_intersex_acc_su_t12, observed).
narrative_ontology:measurement(wcat_intersex_acc_su_t18, woman_category__intersex_accommodation_reading, suppression_requirement, 18, 0.6).
narrative_ontology:measurement_basis(wcat_intersex_acc_su_t18, observed).
narrative_ontology:measurement(wcat_intersex_acc_su_t24, woman_category__intersex_accommodation_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement_basis(wcat_intersex_acc_su_t24, observed).
narrative_ontology:measurement(wcat_intersex_acc_su_t30, woman_category__intersex_accommodation_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement_basis(wcat_intersex_acc_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__intersex_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'who counts as a woman' decomposes into three structurally distinct constraints - one per reading of the woman_category kernel - each with its own epsilon, victim set, and classification, per the epsilon-invariance principle. The sex_biology_reading is the inherited upstream default (longest institutional entrenchment) and is cited as authority by both challengers' opponents; this intersex_accommodation_reading exerts downstream pressure on the gender_identity_reading by forcing it to specify its biology handling (pure identity criteria must say what happens when identity and biology diverge at the sports edge), while the gender_identity_reading reciprocally pressures this reading by exposing the residual exclusions its biology-fit line still produces. Additionally, this story may itself decompose along the civil/sport site axis documented in the domain_concentration_decomposition omega; if split, the two child stories inherit this family's edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_category__intersex_accommodation_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
