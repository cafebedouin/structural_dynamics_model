% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__individual_right_reading, []).

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
 *   constraint_id: second_amendment_boundary__individual_right_reading
 *   human_readable: Second Amendment Individual-Right Boundary (Operative-Clause Reading)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story instantiates the individual_right_reading of the
 *   second_amendment_boundary kernel as binding law: since Heller (2008) and
 *   especially Bruen (2022), the operative clause is read to establish a
 *   pre-existing individual right whose scope the prefatory militia clause
 *   announces but does not limit, and regulation is judged against
 *   founding-era text, history, and tradition rather than by means-end
 *   balancing. The standing arrangement under contest — the thing this story
 *   is about — is that regime in operation: a protected domain around private
 *   possession, presumptive invalidity for a wide class of regulatory
 *   instruments, and a commercially shielded market, with costs concentrating
 *   on populations exposed to firearm violence. Per the epsilon-referent
 *   rule, epsilon is authored for this standing arrangement as this reading's
 *   own lights assess it; the reading's endorsed alternative (or its
 *   siblings' alternatives) is not the referent. The claimed type and the
 *   metrics are independent authored facts: I claim tangled_rope because the
 *   arrangement demonstrably carries both a genuine liberty-coordination
 *   function and asymmetric cost-bearing through the same structure,
 *   sustained by active judicial enforcement; the metrics describe the
 *   arrangement's operation as the record shows it. Where the engine's
 *   per-seat computations diverge from this claim, that divergence is the
 *   datum.
 *
 * KEY AGENTS:
 *   - federal_courts: agenda-setting enforcer (institutional/mobile) — authors and administers the boundary test, bears none of its costs
 *   - law_abiding_gun_owners: primary beneficiary (organized/identity_locked) — holds the protected possession domain
 *   - firearms_manufacturers_retailers: primary beneficiary (institutional/arbitrage) — holds the constitutionally shielded market
 *   - gun_rights_advocacy_organizations: beneficiary and litigation driver (organized/mobile) — monetizes and steers the contest
 *   - mass_shooting_victims: primary target (powerless/trapped) — bears uncompensated casualty costs
 *   - domestic_violence_victims: primary target (powerless/trapped) — bears elevated intimate-partner homicide risk
 *   - firearm_suicide_decedents: primary target (powerless/trapped) — bear the largest single mortality share
 *   - state_legislatures_and_regulators: secondary target (institutional/constrained) — lose regulatory instruments to invalidation
 *   - gun_control_advocacy_organizations: excluded voice (organized/trapped) — empirical case has no doctrinal entry point
 *   - public_health_research_community: analytical observer (moderate/analytical) — produces the unweighed evidence
 *   - general_public: diffuse bearer (moderate/constrained) — absorbs statistical risk and fiscal costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, 0.68).
domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, 0.66).
domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Second Amendment Individual-Right Boundary (Operative-Clause Reading)").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, '199ec466-deef-48ef-86c5-2389e3c40573').
narrative_ontology:cs_kernel_codification('199ec466-deef-48ef-86c5-2389e3c40573', fixed_text).
narrative_ontology:cs_authority_grounding('199ec466-deef-48ef-86c5-2389e3c40573', lineage).
narrative_ontology:cs_interpretation_layer_present('199ec466-deef-48ef-86c5-2389e3c40573').
narrative_ontology:cs_reading_relation('199ec466-deef-48ef-86c5-2389e3c40573', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_reading_relation('199ec466-deef-48ef-86c5-2389e3c40573', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('199ec466-deef-48ef-86c5-2389e3c40573', foundational, right_precedes_ratification).
narrative_ontology:cs_axiom_status(right_precedes_ratification, holdable).
narrative_ontology:cs_axiom_grounding('199ec466-deef-48ef-86c5-2389e3c40573', right_precedes_ratification, deontological).
narrative_ontology:cs_axiom('199ec466-deef-48ef-86c5-2389e3c40573', foundational, prefatory_clause_announces_purpose_only).
narrative_ontology:cs_axiom_status(prefatory_clause_announces_purpose_only, holdable).
narrative_ontology:cs_axiom_grounding('199ec466-deef-48ef-86c5-2389e3c40573', prefatory_clause_announces_purpose_only, conventional).
narrative_ontology:cs_reference_frame('199ec466-deef-48ef-86c5-2389e3c40573', pre_existing_individual_liberty_declaration).
narrative_ontology:cs_drift_state('199ec466-deef-48ef-86c5-2389e3c40573', post_bruen_text_history_tradition_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('199ec466-deef-48ef-86c5-2389e3c40573', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__individual_right_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, law_abiding_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_manufacturers_retailers).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, mass_shooting_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, domestic_violence_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, firearm_suicide_decedents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, general_public).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, state_legislatures_and_regulators).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, general_public).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, pre_existing_individual_right_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, text_history_tradition_interpretive_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Supreme Court authored the operative test in District of Columbia v. Heller (2008) and New York State Rifle & Pistol Association v. Bruen (2022), and the circuits administer it case by case. It can narrow or widen the boundary by choosing which petitions to grant; recent grants on novel weapon technologies show active management of the line. It bears none of the arrangement's costs and can revise its own precedents.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, federal_courts, agenda_setter,
    institutional, civilizational, mobile, national).

% Tens of millions of households keep handguns and long guns for self-defense, hunting, and sport. The constitutional guarantee secures their possession against licensing regimes, category bans, and storage mandates that other democracies sustain. For many, ownership is bound up with self-concept, rural community standing, and family inheritance; relinquishing it is experienced as shedding an identity rather than switching products.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, law_abiding_gun_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Produce and sell the goods whose possession the boundary protects. The shield converts potential product bans, strict-liability regimes, and outright prohibitions into presumptively invalid measures, giving the industry demand certainty no comparable consumer sector enjoys. Firms respond to adverse state markets by diversifying product lines, shifting distribution, and pursuing export sales rather than exiting.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_manufacturers_retailers, beneficiary,
    institutional, generational, arbitrage, global).

% Fund test cases, file amicus briefs, recruit plaintiffs, and mobilize members. Membership and donation revenue track the perceived immediacy of threats to the boundary, so organizational health is tied to the persistence of the contest itself. They choose venues and fact patterns, shaping which questions reach the courts and in what posture.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, gun_rights_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__individual_right_reading, gun_rights_advocacy_organizations, agenda_setter).

% People killed or wounded in multi-victim shootings, and their families. They bear medical, funeral, and lifetime disability costs with no compensating channel from the arrangement that kept the instrument accessible. Their preferred protections have repeatedly been enacted at state level and then enjoined or invalidated; they cannot exit the risk environment short of avoiding all public gathering.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, mass_shooting_victims, payer,
    powerless, biographical, trapped, national).

% Face partners or ex-partners with firearm access; the presence of a gun in a domestic-violence household multiplies homicide risk, and separation is the most dangerous moment. Disarmament of subjects of restraining orders survived only narrowly in United States v. Rahimi (2024) and remains contested in the lower courts. Leaving the relationship is their principal safety strategy, and it is the one the arrangement makes least reliable.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, domestic_violence_victims, payer,
    powerless, immediate, trapped, national).

% Roughly half of United States firearm deaths are suicides. Access to a firearm during a crisis converts transient intent into death; the policies most directly aimed at this channel — waiting periods and extreme-risk protection orders — are among the most frequently challenged measures. The people who bore this cost are not available to testify; their families and the mortality record speak for them.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearm_suicide_decedents, payer,
    powerless, immediate, trapped, national).

% Enact permitting schemes, category restrictions, and carry regulations responsive to their electorates. Since Bruen a large share are enjoined or invalidated under the historical-analogue test. They absorb the cost of drafting, defending, and losing litigation, of enforcement staff idled under injunctions, and of re-legislating within a shrinking space of permissible instruments. They cannot opt out of federal constitutional supremacy.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, state_legislatures_and_regulators, payer,
    institutional, generational, constrained, regional).

% Pursue regulation through legislatures and ballot initiatives. The governing interpretive frame admits no interest-balancing, so their empirical case — mortality data, intervention trials, comparative law — has no doctrinal entry point. They operate as permanent litigants seeking exceptions at the margin rather than participants in the test that decides outcomes.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, gun_control_advocacy_organizations, excluded,
    organized, generational, trapped, national).

% Bears diffuse exposure to firearm homicide, accident, and suicide contagion, and funds the policing and emergency-care consequences through taxation. A minority of households report defensive uses of firearms. No individual can opt out of the ambient risk environment; the costs arrive statistically, spread across millions who never appear in any case caption.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, general_public, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__individual_right_reading, general_public, beneficiary).

% Produces the mortality surveillance and intervention-effectiveness evidence. Operated for two decades under appropriations riders restricting federally funded advocacy-adjacent research, rebuilding through private and state funding. Supplies the empirical record that the historical test does not weigh, and documents the outcome differences between jurisdictions that the doctrine treats as constitutionally irrelevant.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, public_health_research_community, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__individual_right_reading, firearms_manufacturers_retailers).
narrative_ontology:fixing_cost_class(second_amendment_boundary__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a stable, judicially administrable line between individual armed self-protection and state police power. It assures a large class of citizens that possession cannot be revoked by shifting majorities, and gives state governments determinate notice of which regulatory instruments are unavailable, so both sides can plan.
% TRANSFER_FUNCTION: Moves regulatory authority from state governments to individual rights-holders; moves mortality, disability, and policing costs onto the general population, concentrated on victims of firearm violence; moves revenue certainty and liability insulation to manufacturers and retailers; moves litigation expenditure from both advocacy camps into the courts.
% ABSENT_VOICES: Gun-violence victims and public-health authorities are absent from the interpretive frame itself: the text-history-tradition test admits no interest-balancing, so their evidence has no doctrinal entry point. They appear only as dissenting opinions, state statutes awaiting invalidation, and amicus briefs the test does not weigh. Their absence is structural, not incidental — the method excludes the kind of argument they would make.
% DISAPPEARANCE_RATIONALE: If the individual-right boundary vanished overnight, states would re-legislate comprehensively — licensing, category bans, storage mandates, carry restrictions — within a few sessions; the commercial market would contract sharply; the political coalition structure built around the boundary (industry, advocacy organizations, identity-committed owners) would reorganize around ordinary statutory politics; and the litigation economy consuming both camps would collapse into normal administrative law.
% FOUNDING_PROBLEM: Guarantee that the people's arms capacity could not be selectively stripped by a hostile government — the founding-era fears of standing armies and of disarmament of disfavored populations — carried forward in modern form as an assurance of individual armed self-defense against state prohibition.
% FOUNDING_PROBLEM_CORROBORATION: Historians outside the advocacy set corroborate the founding problem's reality: founding- and Reconstruction-era records document both the anti-standing-army concern and recurring episodes of selective disarmament of disfavored groups. Public-health researchers corroborate that the modern harm profile — handgun ubiquity, firearm suicide as the majority share of gun deaths, mass-casualty events — differs structurally from the founding-era problem the guarantee addressed. Each camp attests the problem it knows; neither attests the other's, which is why the status is contested rather than live or dead.
narrative_ontology:disappearance_verdict(second_amendment_boundary__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_boundary__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__individual_right_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68: the arrangement transfers mortality, disability, and fiscal costs onto identifiable, uncompensated populations while insulating a possession domain and a commercial market — but it is not maximal, because genuine defensive-use and liberty benefits flow to millions of holders and Rahimi (2024) narrowed the edge (domestic-violence disarmament survived). Suppression 0.66: enforcement runs through pre-enforcement injunctions, fee-shifting exposure, and the chilling of legislative action — structural mechanisms, not internalized ones; a minority internalized component exists as reformer fatalism, but the operative force is external. Theater 0.50: the historical-analogy method performs archival neutrality while lower-court outcomes cluster predictably; roughly half of doctrinal activity defends the boundary's administration rather than resolving genuinely open questions. Accessibility_collapse 0.62: post-Bruen, may-issue licensing, broad category bans, and interest-balancing defenses collapsed, but Rahimi preserved a class of disarmament measures, so alternatives are severely diminished rather than eliminated. Resistance 0.60: continuous — post-mass-shooting legislative surges, state re-legislation designed to invite test cases, and ballot measures. Measurement design: one shared seven-point grid (t=0..18, mapping approximately to 2008..2026) with all three series authored at every point; trajectories rise monotonically with a Rahimi inflection at t=15 to t=18. The rising base_extractiveness series is authored deliberately so the T17 accumulation trigger can read it. Coalition note: the victim seats are individually powerless; coalition attempts (mass mobilization, ballot measures) have moved legislatures but not doctrine, because the boundary is constitutionally entrenched against ordinary majorities — coalition energy converts into litigation, which the frame channels into history rather than balancing.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat (federal_courts), the arrangement is a neutral methodology faithfully recovering founding meaning; from the victim seats, the same methodology is a machine for presumptive invalidity that admits no countervailing evidence; from the owner seat, it is a restored pre-existing right; from the industry seat, it is demand certainty; from the research seat, it is a decision procedure that renders the relevant evidence irrelevant. Same-level divergence among beneficiaries: gun owners sit identity_locked (ownership fused with self-concept and community standing) while manufacturers sit arbitrage (product-line and geographic flexibility) — equal nominal beneficiary position, radically different exit profiles, so the engine should compute different effective positions for them. The advocacy organizations introduce a further wrinkle: their revenue tracks threat salience, so the seat that most loudly defends the boundary also has an ongoing interest in the contest never concluding.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the owner, industry, and advocacy seats near the subsidized end: the protected domain and shielded market flow to them, and none bears the arrangement's casualty costs. Owners sit nearest the subsidy pole (full enjoyment, identity-locked retention); industry sits marginally inward of pure subsidy (competitive exposure within the market); advocacy organizations collect from the contest itself. The three victim declarations place those seats near the full-target end, and their trapped exits push them further along that axis — there is no purchase on the arrangement from inside the harm. The regulator seat derives a high target-position from its payer role and constrained exit (it cannot secede from constitutional supremacy). The general_public seat is genuinely dual-positioned and should compute near symmetric: diffuse statistical burden against a minority defensive-use benefit. The court seat sits near the beneficiary pole as author and administrator — it collects doctrinal authority from the arrangement's operation. No directionality overrides are used: the beneficiary/victim declarations plus exit profiles produce the correct qualitative placement for every seat, and the schema's override mechanism keys on power atoms, which would misfire here because institutional seats point in opposite directions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a guarantee against selective disarmament — retains live adherents, and historians outside the advocacy set corroborate both its founding-era reality and the structural difference of the modern harm profile; hence founding_problem_status is contested, not dead. Cross-check: contested status paired with a world_rearranges disappearance verdict produces no dead-mandate mismatch flag — the arrangement persists because its mandate still has a constituency, not because anyone is administering a corpse. The classification discipline matters in both directions here: reading the arrangement as pure coordination would erase the victim seats and the market shield from the ledger; reading it as pure extraction would erase the liberty function that anchors a mass constituency and that courts articulate in good faith. The hybrid keeps both surfaces visible and forces the divergence into the per-seat computation, where it belongs. Identity-lock dynamics: the owner seat's exit is fused ideologically and communally — ownership signifies self-sufficiency and belonging, so exit is unthinkable rather than merely costly; if that identity frame broke (ownership reframed as a consumer product choice), the beneficiary coalition would thin rapidly and the arrangement's political maintenance cost would rise sharply.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates one reading (individual_right_reading) of the second_amendment_boundary kernel; which structural elements would change under the sibling readings, and where exactly is the disagreement located?',
    'Comparative classification across the three sibling stories: if the militia-conditioned sibling yields a materially different victim set and regulatory domain, the disagreement is located in the prefatory clause''s limiting force; if the insurrectionist sibling relocates the right''s foundation, the disagreement is located in the right''s grounding rather than its scope.',
    'This story''s classification is stable under its own reading; merging readings would average incompatible victim sets and destroy epsilon-invariance — the family must remain decomposed, and any corpus-level verdict on ''the Second Amendment'' must be assembled from the three files, not substituted by any one of them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this constraint is one of three live readings of the Second Amendment kernel.').

omega_variable(
    pre_existing_right_status,
    'Is the individual right genuinely pre-existing — a natural right the text acknowledges — or a construct of interpretive choice that the text permits but does not compel?',
    'Founding-era historiography on arms regulation (local laws disarming Catholics, enslaved people, and loyalists; carry restrictions in settled towns) read alongside the natural-rights tradition the ratifying generation explicitly invoked.',
    'If constructed, the boundary is a policy choice wearing natural-law dress and the arrangement sits nearer the extractive end of the hybrid range; if genuinely pre-existing, part of the measured burden is the price of a liberty guarantee rather than imposed cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_existing_right_status, conceptual, 'Natural-right versus constructed status of the asserted pre-existing right.').

omega_variable(
    counterfactual_mortality_attribution,
    'How much firearm mortality is attributable to the constitutional shield itself, versus what would persist under the comprehensive regulation the militia-conditioned sibling reading would permit?',
    'Panel studies exploiting state policy variation, post-Bruen carry-surges as natural experiments, and cross-national comparisons adjusted for underlying violence rates.',
    'A large attributable share raises the effective burden on the victim seats and supports the extraction-heavy reading of the arrangement; a small share supports this reading''s claim that criminal misuse, not the boundary, produces the harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_mortality_attribution, empirical, 'Causal share of firearm deaths attributable to the shielded-access arrangement.').

omega_variable(
    defensive_use_offset,
    'How large is lawful defensive firearm use, and does it offset the harm borne by the victim seats?',
    'Reconciling survey-based estimates (orders of magnitude higher) with incident-based victimization counts; measuring the severity of harms prevented, not merely their frequency.',
    'A large verified offset pushes the arrangement toward net coordination and lowers effective extraction; a small offset leaves the victim seats bearing uncompensated net costs and firms the tangled_rope classification against rope-flavored reinterpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(defensive_use_offset, empirical, 'Magnitude and quality of defensive-use benefits offsetting victim costs.').

omega_variable(
    rahimi_narrowing_durability,
    'Does United States v. Rahimi (2024) mark a durable narrowing principle — historical limits on who may be disarmed — or a one-off carve-out preceding renewed extension?',
    'Track certiorari grants and circuit application over the coming decade: whether the responsible-citizen language hardens into a categorical limit or dissolves case by case.',
    'Durable narrowing flattens the suppression and extractiveness trajectories; dissolution restores the rising path and pushes the arrangement toward the extraction-heavy end of the hybrid range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rahimi_narrowing_durability, empirical, 'Durability of the post-Bruen narrowing inflection visible at t=15 to t=18.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__individual_right_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t3, second_amendment_boundary__individual_right_reading, theater_ratio, 3, 0.29).
narrative_ontology:measurement_basis(seco_tr_t3, observed).
narrative_ontology:measurement(seco_tr_t6, second_amendment_boundary__individual_right_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement_basis(seco_tr_t6, observed).
narrative_ontology:measurement(seco_tr_t9, second_amendment_boundary__individual_right_reading, theater_ratio, 9, 0.39).
narrative_ontology:measurement_basis(seco_tr_t9, observed).
narrative_ontology:measurement(seco_tr_t12, second_amendment_boundary__individual_right_reading, theater_ratio, 12, 0.44).
narrative_ontology:measurement_basis(seco_tr_t12, observed).
narrative_ontology:measurement(seco_tr_t15, second_amendment_boundary__individual_right_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement_basis(seco_tr_t15, observed).
narrative_ontology:measurement(seco_tr_t18, second_amendment_boundary__individual_right_reading, theater_ratio, 18, 0.5).
narrative_ontology:measurement_basis(seco_tr_t18, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__individual_right_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t3, second_amendment_boundary__individual_right_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement_basis(seco_be_t3, observed).
narrative_ontology:measurement(seco_be_t6, second_amendment_boundary__individual_right_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement_basis(seco_be_t6, observed).
narrative_ontology:measurement(seco_be_t9, second_amendment_boundary__individual_right_reading, base_extractiveness, 9, 0.62).
narrative_ontology:measurement_basis(seco_be_t9, observed).
narrative_ontology:measurement(seco_be_t12, second_amendment_boundary__individual_right_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement_basis(seco_be_t12, observed).
narrative_ontology:measurement(seco_be_t15, second_amendment_boundary__individual_right_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement_basis(seco_be_t15, observed).
narrative_ontology:measurement(seco_be_t18, second_amendment_boundary__individual_right_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement_basis(seco_be_t18, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__individual_right_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(seco_su_t0, observed).
narrative_ontology:measurement(seco_su_t3, second_amendment_boundary__individual_right_reading, suppression_requirement, 3, 0.43).
narrative_ontology:measurement_basis(seco_su_t3, observed).
narrative_ontology:measurement(seco_su_t6, second_amendment_boundary__individual_right_reading, suppression_requirement, 6, 0.49).
narrative_ontology:measurement_basis(seco_su_t6, observed).
narrative_ontology:measurement(seco_su_t9, second_amendment_boundary__individual_right_reading, suppression_requirement, 9, 0.56).
narrative_ontology:measurement_basis(seco_su_t9, observed).
narrative_ontology:measurement(seco_su_t12, second_amendment_boundary__individual_right_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement_basis(seco_su_t12, observed).
narrative_ontology:measurement(seco_su_t15, second_amendment_boundary__individual_right_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(seco_su_t15, observed).
narrative_ontology:measurement(seco_su_t18, second_amendment_boundary__individual_right_reading, suppression_requirement, 18, 0.66).
narrative_ontology:measurement_basis(seco_su_t18, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, second_amendment_boundary__militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, second_amendment_boundary__insurrectionist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Second Amendment.' The natural-language concept covers three structurally distinct claims that differ on the prefatory clause's limiting force and on the right's foundation; per the epsilon-invariance principle they are authored as separate stories with separate epsilon values, victim sets, and classifications, linked here. The individual-right reading is the upstream member in the institutional sense — it is the operative law, so its doctrine shapes the legitimacy conditions and resource environment in which the sibling readings are argued (militia-conditioned advocates litigate within a frame their reading rejects; insurrectionist claims borrow the individual-right victory rhetorically while resting on a different foundation). Sibling files should carry reciprocal edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
