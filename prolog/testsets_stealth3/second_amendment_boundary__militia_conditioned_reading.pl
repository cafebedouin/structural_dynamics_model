% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__militia_conditioned_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__militia_conditioned_reading, []).

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
 *   constraint_id: second_amendment_boundary__militia_conditioned_reading
 *   human_readable: Militia-Conditioned Boundary on Arms Possession (Prefatory-Clause-Scope Reading)
 *   domain: constitutional/political/firearms_policy
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested constitutional
 *   kernel second_amendment_boundary: the militia-conditioned reading, under
 *   which the prefatory 'well regulated Militia' clause defines the scope of
 *   the operative right, leaving possession of arms outside organized
 *   collective defense without constitutional protection and weapons
 *   regulation presumptively valid legislative business. The standing
 *   arrangement under contest — the epsilon referent — is that doctrinal
 *   regime itself, assessed by this reading's own lights: burdens on
 *   non-militia possessors register as legitimate democratic policy costs
 *   rather than takings, so epsilon is authored moderate-low even though the
 *   victim set is real and named. A sibling reading of the same referent
 *   would author substantially higher epsilon; the difference is
 *   reading-indexed, and the two stories are constraint-family members, not
 *   one constraint with a measurement dial. The claim/metric gap is
 *   deliberate: claimed_type tangled_rope asserts a structure that genuinely
 *   coordinates (democratic arms governance, disciplined collective capacity)
 *   while the same doctrine strips a defined class of constitutional recourse
 *   and requires continuous enforcement. Interval indexing: T=0 sits near the
 *   1903 federalization of the militia; T=36 marks the 1934-39 federal
 *   regulatory build-out and the militia-scoped Supreme Court articulation;
 *   T=72 the enforcement peak of the early 1970s; T=108 the 2008 judicial
 *   repudiation of the reading as a matter of national doctrine. KEY AGENTS
 *   (by structural relationship): - state_regulatory_authorities:
 *   agenda-setter and institutional beneficiary
 *   ([institutional]/[constrained]) — enacts and administers the regulatory
 *   permission the reading preserves; collects fees and enforcement mandate -
 *   organized_state_militias_national_guard: primary constitutional
 *   beneficiary ([institutional]/[constrained]) — holds the arm-bearing
 *   capacity the reading reserves - urban_gun_violence_exposed_communities:
 *   beneficiary ([organized]/[trapped]) — receives the public-safety product
 *   of comprehensive regulation - non_militia_private_possessors: primary
 *   target ([organized]/[constrained]) — bears restriction without
 *   constitutional recourse -
 *   self_defense_claimants_restrictive_jurisdictions: primary target
 *   ([powerless]/[trapped]) — bears prohibition where defensive need is
 *   highest - firearms_commerce_participants: secondary target
 *   ([powerful]/[arbitrage]) — market exposed to democratic restriction -
 *   historically_excluded_muster_groups: excluded voice
 *   ([powerless]/[trapped]) — administered out of the militia and thus out of
 *   the right's protection - constitutional_interpreters: analytical observer
 *   ([analytical]/[analytical]) — produces and sustains the doctrinal record
 *
 * KEY AGENTS:
 *   - state_regulatory_authorities: agenda-setter and institutional beneficiary ([institutional]/[constrained]) — enacts and administers the regulatory permission; collects fees and enforcement mandate
 *   - organized_state_militias_national_guard: primary constitutional beneficiary ([institutional]/[constrained]) — holds the reserved arm-bearing capacity
 *   - urban_gun_violence_exposed_communities: beneficiary ([organized]/[trapped]) — receives the public-safety product
 *   - non_militia_private_possessors: primary target ([organized]/[constrained]) — bears restriction without constitutional recourse
 *   - self_defense_claimants_restrictive_jurisdictions: primary target ([powerless]/[trapped]) — bears prohibition where defensive need is highest
 *   - firearms_commerce_participants: secondary target ([powerful]/[arbitrage]) — market exposed to restriction
 *   - historically_excluded_muster_groups: excluded voice ([powerless]/[trapped]) — administered out of militia and protection alike
 *   - constitutional_interpreters: analytical observer ([analytical]/[analytical]) — sustains the doctrinal record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, 0.28).
domain_priors:suppression_score(second_amendment_boundary__militia_conditioned_reading, 0.44).
domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__militia_conditioned_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Militia-Conditioned Boundary on Arms Possession (Prefatory-Clause-Scope Reading)").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional/political/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__militia_conditioned_reading, '6f1f7470-af8d-4174-8758-bd0f5a2b6a58').
narrative_ontology:cs_kernel_codification('6f1f7470-af8d-4174-8758-bd0f5a2b6a58', fixed_text).
narrative_ontology:cs_authority_grounding('6f1f7470-af8d-4174-8758-bd0f5a2b6a58', lineage).
narrative_ontology:cs_interpretation_layer_present('6f1f7470-af8d-4174-8758-bd0f5a2b6a58').
narrative_ontology:cs_reading_relation('6f1f7470-af8d-4174-8758-bd0f5a2b6a58', second_amendment_boundary__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('6f1f7470-af8d-4174-8758-bd0f5a2b6a58', second_amendment_boundary__insurrectionist_reading, influences).
narrative_ontology:cs_axiom('6f1f7470-af8d-4174-8758-bd0f5a2b6a58', foundational, prefatory_clause_bounds_operative_scope).
narrative_ontology:cs_axiom_status(prefatory_clause_bounds_operative_scope, holdable).
narrative_ontology:cs_axiom_grounding('6f1f7470-af8d-4174-8758-bd0f5a2b6a58', prefatory_clause_bounds_operative_scope, empirically_contingent).
narrative_ontology:cs_axiom('6f1f7470-af8d-4174-8758-bd0f5a2b6a58', secondary, arms_regulation_within_police_power).
narrative_ontology:cs_axiom_status(arms_regulation_within_police_power, holdable).
narrative_ontology:cs_axiom_grounding('6f1f7470-af8d-4174-8758-bd0f5a2b6a58', arms_regulation_within_police_power, conventional).
narrative_ontology:cs_reference_frame('6f1f7470-af8d-4174-8758-bd0f5a2b6a58', prefatory_scope_disciplined_grant).
narrative_ontology:cs_drift_state('6f1f7470-af8d-4174-8758-bd0f5a2b6a58', post_heller_doctrine_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('6f1f7470-af8d-4174-8758-bd0f5a2b6a58', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, organized_state_militias_national_guard).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, urban_gun_violence_exposed_communities).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_regulatory_authorities).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, non_militia_private_possessors).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, self_defense_claimants_restrictive_jurisdictions).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_commerce_participants).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, prefatory_clause_scope_canon).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, civic_republican_arms_theory).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, police_power_primacy_over_weapons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures and administrative agencies that enact and run licensing, registration, waiting periods, category restrictions, and dealer oversight. The prevailing constitutional reading leaves these decisions to them without judicial veto; agencies collect application and license fees and request enforcement appropriations, and officials build careers administering the resulting bureaucracy. Reversal of the reading would shrink both their discretion and their budget.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_regulatory_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__militia_conditioned_reading, state_regulatory_authorities, beneficiary).

% Uniformed, disciplined formations under dual state-federal command. Drill, armory storage, and lawful training constitute the arm-bearing activity the prevailing reading recognizes as constitutionally anchored. Members bear arms as an office of civic service rather than an unrestricted personal entitlement; the formation's leadership answers to governors and the President, not to private preference.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, organized_state_militias_national_guard, beneficiary,
    institutional, generational, constrained, national).

% Neighborhoods with concentrated firearm homicide exposure. Residents vote, organize, and petition for licensing requirements, purchase limits, and removal of illegal guns; when such measures hold, fewer guns circulate on their streets. Protection arrives collectively; individually they cannot buy their way out of exposure and rarely relocate.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, urban_gun_violence_exposed_communities, beneficiary,
    organized, biographical, trapped, regional).

% Collectors, competitors, and recreational shooters who hold firearms apart from any organized formation. Their holdings carry no constitutional shield against prohibition or confiscation under the prevailing reading; they comply with whatever their state enacts, relocate at real cost, fund advocacy organizations, or await judicial reversal. A substantial segment fuses the hobby and its culture with personal identity, which makes departure feel like self-erasure even where moving is feasible.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, non_militia_private_possessors, payer,
    organized, biographical, constrained, national).

% Residents of dense, high-violence, heavily regulating cities who seek a firearm for defense of home and person. The prevailing reading leaves their access entirely to legislative discretion; they face permit denials, long waits, or outright prohibition while remaining exposed to the violence that motivated the request. Moving away is usually impossible; hiring lawyers rarely helps; their claim finds no receptive forum.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, self_defense_claimants_restrictive_jurisdictions, payer,
    powerless, biographical, trapped, local).

% Manufacturers, importers, and dealers whose product lines, marketing, and retail footprint depend on what legislatures allow. Restrictions and audits remove categories and customers quickly. The industry lobbies, litigates, redesigns products to fit new rules, and shifts inventory between permissive and restrictive states; demand also spikes whenever restriction looms.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_commerce_participants, payer,
    powerful, biographical, arbitrage, national).

% Communities denied a place in the militia itself — through racial exclusion, discriminatory enlistment administration, or prohibition on arming — during the era when militia membership mediated lawful access to arms. Shut out of the formation and of the protection said to flow through it, they were disarmed by the same instrument that armed their neighbors. Descendant communities carry the legacy; their objection to the collective boundary was never heard inside the doctrine's own deliberations.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, historically_excluded_muster_groups, excluded,
    powerless, generational, trapped, national).

% Judges, professors, and lawyers who decide what the amendment's two clauses mean and how much weight each carries. Under the prevailing reading they treat the opening clause as load-bearing, defer weapons regulation to legislatures, and reject petitions framing private possession as a protected natural entitlement. Careers, reputations, and law-school curricula were built on sustaining or attacking that method.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, constitutional_interpreters, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__militia_conditioned_reading, state_regulatory_authorities).
narrative_ontology:fixing_cost_class(second_amendment_boundary__militia_conditioned_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Routes the polity's armed capacity into disciplined, accountable formations and leaves decisions about civilian weapons to ordinary legislation, so that neither the federal government nor a judicially frozen settlement dictates who may be armed. It solves the founding-era problem of securing common defense without a large standing army, and the recurring problem of letting majorities govern instruments of lethal force.
% TRANSFER_FUNCTION: Moves regulatory discretion, fee revenue, and enforcement budgets to state legislatures and agencies; moves security of possession away from private holders — collectors, competitors, defensive seekers — who lose recourse when legislatures restrict; moves control of the firearms market from sellers and makers to electoral majorities.
% ABSENT_VOICES: Those administered out of the militia — and therefore out of the right — sat outside every room where the doctrine was maintained: excluded from muster rolls by race, then unable to invoke a right defined as membership in the body that excluded them. Their objections survive only in historical scholarship, not in the doctrinal record the interpreters produced. Commentary-grade absence only; it does not enter the engine's classification arithmetic.
% DISAPPEARANCE_RATIONALE: Deletion of the militia-conditioned boundary overnight would reopen every closed door at once: courts would begin entertaining challenges to licensing schemes, bans, and permit systems that currently stand uncontested; possessors would acquire a shield they now lack; regulators would lose discretion they price and budget around; and the interpretive profession would rebuild its method around whichever successor reading prevailed. Nothing in the firearms economy is indifferent to which clause carries the meaning — the world rearranges.
% FOUNDING_PROBLEM: Secure the states' capacity to arm and organize their own citizen soldiery against a potentially hostile federal center — the founders' substitute for a feared standing army — while keeping day-to-day policing of weapons a local, legislative matter.
% FOUNDING_PROBLEM_CORROBORATION: Military historians document that the 1903 Dick Act converted the constitutional militia into the federally organized, federally equipped National Guard, dissolving the independent state force this arrangement was built to shelter; the Supreme Court itself confirmed in Perpich v. Department of Defense (1990) that the Guard belongs to the national chain of command. Neither source sits inside the reading's beneficiary coalition, and even the reading's loyalists concede the institutional transformation while defending the clause grammar. No corroborator outside the reading's defenders attests that the founding problem remains live.
narrative_ontology:disappearance_verdict(second_amendment_boundary__militia_conditioned_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__militia_conditioned_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__militia_conditioned_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_boundary__militia_conditioned_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__militia_conditioned_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__militia_conditioned_reading_tests).
:- end_tests(second_amendment_boundary__militia_conditioned_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.28 from this reading's own lights over the fixed referent (the standing militia-conditioned regime): the reading regards restriction of non-militia possession as legitimate legislative policy rather than taking, so the burden side registers as policy cost, not theft; the residual 0.28 reflects fee and budget capture by the regulatory apparatus, compliance costs pushed onto makers and dealers, and the historical record of the permission structure being turned to selective disarmament. Suppression (0.44) is a raw structural property, deliberately unscaled: felony-grade penalties, confiscations, and permit denial coerce compliance, but they enforce democratically selected policy rather than shielding rents. Accessibility collapse (0.62): once the doctrine is understood, constitutional-challenge alternatives collapse almost completely — petitions framed as individual entitlement fail by construction — while political and geographic alternatives remain partly open, so collapse is high but short of natural-law completeness. Resistance (0.70) is the century-long scholarly and political counter-current the doctrine absorbed without conceding. Theater (0.38, rising from 0.15) tracks the referent problem: the Militia the clause invokes was dissolved into the federally commanded National Guard in 1903, so a growing share of the doctrine's maintenance is rhetorical reconstruction of an institution that no longer exists in its founding form. All three series share one time grid ({0, 18, 36, 54, 72, 90, 108}) as the alignment rule requires. Suppression_requirement is authored because the story specifically traces enforcement-capacity change: federal machinery built up through 1934 and 1968, peaked near T=72, then decayed as shall-issue diffusion and pre-repudiation judicial drift shrank the enforced domain. Extractiveness peaks mid-interval (0.40 at T=72), when deference was absolute and the burdened set widest, then declines as the regime's reach contracts ahead of its repudiation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different worlds from identical text. From the regulatory seat the arrangement is self-government: legislatures answering constituents, agencies executing mandates, fees covering administration — a functioning coordination its occupants would deny contains anyone's plunder. From the trapped defensive seeker's seat the same doctrine is a locked door with no appeal: her request fails by construction, her neighborhood stays dangerous, and exit is fictitious. From the possessor organizations' seat it is a siege to be reversed politically; from the industry's seat a portfolio risk to be arbitraged. Identity fusion sharpens the divergence: for possessors who have made marksmanship and armament constitutive of self, the boundary reads as an attack on personhood rather than a policy preference, which is why resistance stayed fierce long after the founding problem died. The engine computes these per-seat classifications from power, exit, and directional data; the authored claim does not referee them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit at low d: Guard formations receive protected status outright; exposed urban communities receive the safety product; regulatory authorities derive discretion, fees, and budgets, with their agenda-setter role compounding the subsidy. Targets sit at high d: non-militia possessors and defensive seekers surrender possession security through the same structure that subsidizes the beneficiaries, and their exit profiles (constrained, trapped) push them toward the full-target end. Commerce participants also bear restriction, but arbitrage-grade exit dampens their effective position. Historically excluded muster groups are seated as excluded rather than as payers — an authored absence is commentary-grade and does not feed the arithmetic. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already place every seat correctly, and the override mechanism keys on power atoms, which would smear any correction across unrelated seats sharing a power level.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem died in 1903 when the militia became the federally commanded Guard; the boundary outlived its object by a century, the canonical setup for misreading this arrangement as a piton (mostly theater, nobody hurt enough to fix it) or a snare (rent collection behind a civic costume). The classification refuses both errors. It is not a piton: the regulatory seat captures real, concentrated receipts — discretion, fees, budgets — so maintenance is interested rather than inertial, and the receipt surface names that seat. It is not a snare: extraction is modest and endorsed as legitimate by the reading's own lights, suppression defends policy rather than rents, and no seat is imprisoned in the arrangement for another's enrichment. Tangled rope is the honest middle: a live coordination core (democratic arms governance) carrying a real, asymmetrically borne cost, sustained by active enforcement, with a dead founding mandate and a rising theater ratio (0.15 to 0.38) marking drift toward obsolescence. The dead founding-problem status paired with a world-that-rearranges verdict flags the zombie risk for corpus consumers without forcing the structure into a category its receipts and victims do not support. The mandatrophy lens prevents the reverse error as well: reading the low epsilon as proof of pure coordination would erase the possessors and defensive seekers who pay through the same structure that pays the regulators.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story is one reading of the kernel second_amendment_boundary; which structural elements would flip if a sibling reading (individual right, insurrectionist) were authoritative instead?',
    'Compare the three family stories'' epsilon, beneficiary/victim sets, and computed seat types; the deltas localize the disagreement to the prefatory clause''s function (scope-defining versus purpose-stating) and to the right''s holder (organized formation versus individual).',
    'Under the individual-right reading the victim set expands to every regulator-facing restriction and epsilon rises sharply; under the insurrectionist reading the beneficiary set shifts to autonomous armed individuals and the regulatory permission inverts into a constitutional barrier.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three sibling readings of a shared kernel; disagreement located in the prefatory clause''s function.').

omega_variable(
    militia_referent_obsolescence,
    'What does ''well regulated Militia'' denote now — the federally integrated National Guard, the statutory unorganized militia, or a founding-era institution with no present-day referent?',
    'Statutory analysis of the federal militia code and the Guard''s command chain, combined with founding-era muster practice, to determine whether any present body satisfies the clause the doctrine leans on.',
    'If only a defunct referent qualifies, the doctrine''s bounding term is empty and its permission structure rests on rhetoric, driving theater_ratio toward the piton band; if the unorganized militia counts, millions of possessors fall inside the right and the victim set collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_referent_obsolescence, conceptual, 'Whether the clause''s bounding term still names a real institution.').

omega_variable(
    founding_era_semantic_evidence,
    'Does founding-era drafting history and period usage support reading the prefatory clause as scope-defining rather than purpose-stating?',
    'Corpus linguistics over 1765-1800 political texts, drafting-record analysis of state constitutional analogues, and rate-of-usage studies, with adversarial replication by both reading camps.',
    'Strong scope-defining evidence stabilizes this reading''s foundational claim against the sibling; decisive purpose-stating evidence empirically undermines the axiom and accelerates engine-computed foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_era_semantic_evidence, empirical, 'Testability of the reading''s load-bearing historical premise.').

omega_variable(
    permission_structure_misuse_history,
    'Was the arrangement''s historical deployment — including disarmament campaigns aimed at freedmen and other disfavored groups — intrinsic to the permission structure itself, or downstream misuse of a legitimately granted power?',
    'Separate cases where restriction served the doctrine''s stated collective-defense function from cases where it targeted groups excluded from that function''s protection; compare enforcement patterns against stated intent.',
    'If misuse is intrinsic (the permission exists to enable selective disarmament), epsilon is understated and the structure lies closer to snare; if misuse is parasitic on an otherwise sound grant, the tangled-rope reading stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(permission_structure_misuse_history, conceptual, 'Whether the historical record indicts the permission structure or its users.').

omega_variable(
    suppression_character_ambiguity,
    'Is the measured suppression coercive enforcement of democratically chosen policy — the ordinary cost of governance — or enforcement that ratchets to protect the regulatory apparatus''s own receipts and discretion?',
    'Test whether enforcement intensity correlates with public-safety outcomes or with agency budget cycles and fee dependence after controlling for underlying violence rates.',
    'If suppression protects receipts, the arrangement drifts snare-ward and the named receipt seat hardens into capture; if it defends policy, suppression remains a governance cost and the tangled-rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_character_ambiguity, empirical, 'Whether the coercive apparatus serves policy or its administrators.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 0, 108).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sabr_mcr_tr_t0, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sabr_mcr_tr_t18, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 18, 0.17).
narrative_ontology:measurement(sabr_mcr_tr_t36, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 36, 0.2).
narrative_ontology:measurement(sabr_mcr_tr_t54, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 54, 0.24).
narrative_ontology:measurement(sabr_mcr_tr_t72, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 72, 0.28).
narrative_ontology:measurement(sabr_mcr_tr_t90, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 90, 0.33).
narrative_ontology:measurement(sabr_mcr_tr_t108, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 108, 0.38).

% Extraction over time
narrative_ontology:measurement(sabr_mcr_be_t0, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 0, 0.26).
narrative_ontology:measurement(sabr_mcr_be_t18, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 18, 0.29).
narrative_ontology:measurement(sabr_mcr_be_t36, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 36, 0.33).
narrative_ontology:measurement(sabr_mcr_be_t54, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 54, 0.37).
narrative_ontology:measurement(sabr_mcr_be_t72, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 72, 0.4).
narrative_ontology:measurement(sabr_mcr_be_t90, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 90, 0.34).
narrative_ontology:measurement(sabr_mcr_be_t108, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 108, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(sabr_mcr_su_t0, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(sabr_mcr_su_t18, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 18, 0.44).
narrative_ontology:measurement(sabr_mcr_su_t36, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 36, 0.52).
narrative_ontology:measurement(sabr_mcr_su_t54, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 54, 0.58).
narrative_ontology:measurement(sabr_mcr_su_t72, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 72, 0.6).
narrative_ontology:measurement(sabr_mcr_su_t90, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 90, 0.52).
narrative_ontology:measurement(sabr_mcr_su_t108, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 108, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__militia_conditioned_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__insurrectionist_reading).

% DUAL FORMULATION NOTE:
% Family member of the second_amendment_boundary kernel decomposition. The colloquial label 'the Second Amendment' covers three structurally distinct constraints — militia-conditioned, individual-right, and insurrectionist readings — with different epsilons, victim sets, and classifications; per the epsilon-invariance principle they are separate stories linked through affects_constraints rather than one story with a measurement parameter. Direction of pressure: this reading's century of deference shaped the legal terrain on which the individual-right sibling grew, and that sibling's ascendance now drives the repudiation recorded in this story's drift state.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
