% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Marriage Authority Fragmentation (Federalist Consociational Reading)
 *   domain: legal/constitutional/family law
 *
 * SUMMARY:
 *   In a multi-religious nation, marriage authority is constitutionally
 *   fragmented across personal law codes maintained by each religious
 *   community, backed by state enforcement of their decisions. This reading
 *   interprets the fragmentation as a deliberate consociational mechanism:
 *   the constitutional framers entrenched minority veto power over any
 *   majoritarian unification effort, thereby protecting religious minorities
 *   from demographic domination while blocking the majority from imposing
 *   uniform family law. The constraint's core claim is that legislative
 *   paralysis is a feature (stability, minority protection) rather than a bug
 *   (justice delayed, intra-community inequality). This is one of five
 *   competing readings of the marriage-authority kernel; the others (communal
 *   autonomy, secularist, gender rights, judicial harmonization) offer
 *   structurally distinct accounts of the same constitutional arrangement.
 *   This reading instantiates a low-extraction rope: genuine coordination
 *   (preventing tyranny of majority) with asymmetric benefit (religious
 *   minorities gain veto; majorities and intra-community subordinated groups
 *   pay).
 *
 * KEY AGENTS:
 *   - Religious minority communities: primary beneficiaries of constitutional entrenchment; retain authority over member marriage norms
 *   - Legislative majority: structurally constrained from unifying marriage law; bears cost of paralysis and jurisdictional confusion
 *   - Women and gender minorities in minority communities: identity-locked payers; bear costs of personal law inequality justified by minority autonomy appeal
 *   - Religious community leaders: de facto agenda-setters; interpret and enforce personal law backed by state power
 *   - Constitutional court: observer seat; navigates tension between minority autonomy and fundamental rights via incremental case-by-case review
 *   - Secular urban majority: constrained by pluralism even when preferring uniform law; can block reform via exit (migration) but not via voting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.28).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.15).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Marriage Authority Fragmentation (Federalist Consociational Reading)").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "legal/constitutional/family law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, 'd6183104-de49-4a51-a2e2-ce6289fdcde1').
narrative_ontology:cs_kernel_codification('d6183104-de49-4a51-a2e2-ce6289fdcde1', formalized).
narrative_ontology:cs_authority_grounding('d6183104-de49-4a51-a2e2-ce6289fdcde1', lineage).
narrative_ontology:cs_interpretation_layer_present('d6183104-de49-4a51-a2e2-ce6289fdcde1').
narrative_ontology:cs_reading_relation('d6183104-de49-4a51-a2e2-ce6289fdcde1', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('d6183104-de49-4a51-a2e2-ce6289fdcde1', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('d6183104-de49-4a51-a2e2-ce6289fdcde1', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('d6183104-de49-4a51-a2e2-ce6289fdcde1', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('d6183104-de49-4a51-a2e2-ce6289fdcde1', foundational, consociational_veto_necessity).
narrative_ontology:cs_axiom_status(consociational_veto_necessity, holdable).
narrative_ontology:cs_axiom_grounding('d6183104-de49-4a51-a2e2-ce6289fdcde1', consociational_veto_necessity, deontological).
narrative_ontology:cs_axiom('d6183104-de49-4a51-a2e2-ce6289fdcde1', foundational, majority_tyranny_constitutional_risk).
narrative_ontology:cs_axiom_status(majority_tyranny_constitutional_risk, holdable).
narrative_ontology:cs_axiom_grounding('d6183104-de49-4a51-a2e2-ce6289fdcde1', majority_tyranny_constitutional_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('d6183104-de49-4a51-a2e2-ce6289fdcde1', constitutional_minority_veto_entrenchment).
narrative_ontology:cs_drift_state('d6183104-de49-4a51-a2e2-ce6289fdcde1', contemporary_rights_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d6183104-de49-4a51-a2e2-ce6289fdcde1', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, religious_minority_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, legislative_majority).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, women_in_minority_communities).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, secular_urban_majority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain authority to govern marriage norms, dissolution, inheritance, and succession within their own communities according to religious law. The fragmentation ensures no single democratic majority can impose a uniform civil code that would override their customary authorities. They do not administer the constraint itself (no agenda-setter role), but they benefit from its persistence by maintaining internal autonomy over family arrangements.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, religious_minority_communities, beneficiary,
    moderate, generational, constrained, national).

% Constrained from enacting a uniform civil code or majoritarian marriage law that would override personal law pluralism. They bear the cost of legislative paralysis: disputes between personal law codes remain unresolved, inter-community marriages create jurisdictional confusion, and reform of oppressive personal law norms proceeds slowly through case-by-case judicial intervention rather than coherent legislation. Structural remedy (UCC) remains blocked by constitutionally-entrenched minority veto.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, legislative_majority, payer,
    institutional, generational, mobile, national).

% Bear costs from personal law provisions (unequal divorce rights, limited inheritance, mandatory arbitration in family disputes) that are justified by appeal to community autonomy. Their voice is structurally muted: community leaders speak for 'tradition,' legislators defer to minority protection, courts invoke deference to personal law. Exit from the marriage or the community itself triggers social ostracism and loss of kinship support. Reform advocates for their equality are framed by proponents of the constraint as threats to minority autonomy.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, women_in_minority_communities, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__federalist_millet_reading, women_in_minority_communities, excluded).

% Subject to personal law pluralism even when they would prefer uniform secular civil law. They pay the transaction cost of jurisdictional confusion when marrying across communities, the cost of tolerating provisions they view as inegalitarian, and the cost of legislative blockade that prevents uniform reform. They have the electoral and social power to change the constraint via constitutional amendment, but doing so requires overcoming the entrenched minority veto that the constraint was designed to establish.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, secular_urban_majority, payer,
    powerful, biographical, mobile, national).

% Interpret and administer personal law within their communities, backed by state enforcement of their judgments. They set marriage norms, adjudicate family disputes, authorize dissolutions. The fragmentation ensures their authority survives majoritarian challenge; they have de facto veto over any unified civil code. They have high exit options because they control the interpretive tradition and can shift its boundaries strategically.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, religious_community_leaders, agenda_setter,
    institutional, generational, arbitrage, national).

% Reviews personal law provisions for constitutional compliance, especially gender equality guarantees. They navigate the tension between respecting minority autonomy and enforcing fundamental rights. Their case-by-case jurisprudence moves the constraint's de facto floor without displacing the formal structure of plural authority. They lack direct power to abolish the constraint (that requires constitutional amendment), but their review shapes its boundaries incrementally.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% Face jurisdictional collision when neither partner's personal law covers both, or when the two personal laws conflict. They must litigate to determine which law applies to their marriage, property, or children. They are excluded from the bargain because the constraint was designed by religious communities negotiating elite-level federalism; inter-community formation was not contemplated. Their disputes often hang unresolved, or are decided by courts invoking ad-hoc choice-of-law rules rather than coherent principle.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, inter_community_couples, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__federalist_millet_reading, religious_community_leaders).
narrative_ontology:fixing_cost_class(marriage_authority__federalist_millet_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents any single democratic majority from imposing a uniform marriage law, thereby protecting the family law autonomy of religious minorities and ensuring they retain customary authority over their members' personal status, dissolution, inheritance, and succession arrangements. Coordinates an elite-bargain federalism where majorities and minorities commit to constitutional-level fragmentation of marriage authority rather than compete for control of a unified code.
% TRANSFER_FUNCTION: Moves authority and legitimacy from potential majoritarian legislation to established religious institutions; trades universal legal predictability and gender-equality enforcement for guaranteed minority autonomy. Costs flow from secular majorities (legislative frustration, jurisdictional confusion, slower reform) and from intra-community subordinated groups (women, LGBTQ individuals) to community authorities and to minority communities collectively.
% ABSENT_VOICES: Women and gender minorities within minority communities, inter-community couples, and secular reformers who would advance a uniform civil code all lack effective voice in the original constitutional bargain that entrenched the constraint. They must petition courts (not legislatures) for remedy, and courts invoke deference to minority autonomy to limit their claims. Legislative minorities can block reform; intra-community minorities cannot.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared overnight, a secular democratic majority would swiftly enact a uniform civil code (already drafted in multiple jurisdictions). Religious minorities would lose de facto veto over family law within their communities; state law would govern marriage, dissolution, inheritance, and succession uniformly. Religious institutions would retain internal authority over ritual and doctrine, but no longer enforce family law through state machinery. The political bargain that has held for generations would collapse.
% FOUNDING_PROBLEM: Post-independence constitutional settlement: prevent majority religious group from imposing its family law on religious minorities; preserve consociational federalism in which no single community dominates the state; allow communities to maintain autonomy over personal status as the price of minority cooperation in a shared state.
% FOUNDING_PROBLEM_CORROBORATION: Minority community leaders and constitutional framers (contemporary historical record) attest the founding problem is live and the constraint is its necessary solution. Gender-equality advocates and judicial reformers attest the founding problem has been superseded by human-rights obligations and that minority autonomy should not override constitutional equality guarantees. Constitutional scholars disagree on whether the original bargain remains valid or has been delegitimized by democratic majoritarianism and rights expansion.
narrative_ontology:disappearance_verdict(marriage_authority__federalist_millet_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__federalist_millet_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__federalist_millet_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).
:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28 at interval end) because the constraint is primarily about authority allocation and veto power, not about concentrating resources or rents. The coordination function is real: majorities and minorities both benefit from constitutional stability even when they disagree on substance. Suppression is minimal (0.15) because the constraint operates through constitutional entrenchment, not through active coercion of compliance — the default legal system enforces it rather than requiring dedicated suppressive machinery. Theater ratio climbs modestly (0.08 to 0.22) as the constraint ages: over 75 years, community leaders increasingly invoke 'minority protection' and 'religious autonomy' rhetorically while tacitly tolerating judge-made erosions of their authority (especially on gender equality). The functional claim has not changed, but the performance of upholding it has become more theatrical as the ground shifts beneath. The measurement series tracks one shared time grid: each metric authored at each time point so temporal analysis has complete data.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (religious community leaders) and the primary beneficiary (religious minority communities) will compute near the beneficiary end of directionality (low d, near-zero effective extraction); the constraint stabilizes their authority. The legislative majority and the intra-community subordinated groups will compute near the target end (high d, bearing costs): they are constrained from the majority's preferred policy (secular law) and from intra-community reform (blocked by community leaders claiming to represent 'autonomy'). The constitutional court sits between: it has high institutional power but must respect both minority autonomy and constitutional equality, creating a genuinely mixed position. The constraint's classification may diverge across seats: minority communities compute it as genuine rope (coordination for mutual benefit), while majorities and intra-community women compute it as tangled rope (coordination cover for asymmetric extraction). This divergence is the measurement the framework exists to take — do not reconcile the seats toward a single type.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (religious minorities) enjoy veto power and authority autonomy without running the legislative machinery — they get the benefit of the bargain (protection from majoritarian override) without the cost of administering a state apparatus. They have constrained exit (cannot abandon their religious identity or community and retain social legitimacy), but high relative power within their domain, giving them d near 0.2 (beneficiary end). Payers (legislative majority, intra-community women) bear costs: the majority is locked out of legislating marriage law despite having electoral majorities; intra-community women are locked into personal law norms justified by appeals to 'autonomy' they did not author. Intra-community women have identity_locked exit (to leave is to leave the community entirely), giving them d near 0.9 (full target end). The legislative majority has mobile exit in theory (constitutional amendment, migration to secular jurisdiction) but facing entrenched veto via constitutional requirement of supermajority or super-consensual amendment, effectively identity_locked in their national context, giving them d near 0.75. Community leaders (agenda-setter) have arbitrage exit: they can reinterpret tradition, selectively enforce, or shift interpretive boundaries, and they control the standard against which their authority is measured. Their d is near 0.1 (heavily subsidized by the constraint).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandatrophy dynamics. The founding problem (prevent tyranny of majority over religious minorities) was live and urgent at constitutional founding. After 75 years, the problem status is contested: minority communities attest it remains live (demographic change and majoritarian legislative pushes toward UCC vindicate the concern), while gender-equality advocates and secular reformers attest the problem is superseded (constitutional equality guarantees and human-rights obligations now provide floor-level protection regardless of religious autonomy, so the cost of pluralism outweighs the benefit). The divergence between founding problem status and disappearance verdict is diagnostic: the constraint would cause massive world-rearrangement if it vanished (immediate legislative consolidation around secular law), yet the problem it was built for is no longer universally recognized as live. This is mandatrophy-adjacent but not full mandatrophy: the constraint retains structural necessity for minority protection (it has not become pure theater), but the burden it imposes on majorities and intra-community groups has grown relative to its benefit. Mandatrophy resolution requires either renewed consensus on the founding problem (demonstrated by successful UCC blockade from minorities) or formal acknowledgment that the problem is dead and the constraint has become an encumbrance (legislative amendment to phase out pluralism). The current state is frozen deadlock: the constraint holds because its beneficiaries have veto, not because new majorities are convinced the founding problem remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_empirical_status,
    'Has the constitutional founding problem — tyranny of majority over religious minorities — remained live across the 75-year interval, or has it been substantially mitigated by democratic norms, rights-expansion, and de facto judicial floors?',
    'Historical analysis of legislative pushes toward UCC, polling data on majority preferences for unified law, documentation of actual threats to minority status (formal legislation, electoral campaigns, regulatory action). Comparative evidence from jurisdictions that enacted UCC: did minority subordination increase?',
    'If the problem remains empirically live (legislatures continue pushing UCC, minorities face genuine majoritarian threat), the constraint''s founding mandate persists and mandatrophy resolution is off the table. If the problem is substantially mitigated (UCC pushes are rhetorical only, constitutional equality floors protect minorities adequately, judicial review prevents the most egregious personal law provisions), the constraint becomes an unnecessary encumbrance and is mandatrophy-adjacent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_empirical_status, empirical, 'Whether the founding problem that justified minority veto has remained empirically present or has been substantially mitigated.').

omega_variable(
    alternative_mechanisms_for_minority_protection,
    'Could the same minority-protection goal (preventing tyranny of majority) be achieved via alternative mechanisms (explicit constitutional floors, judicial review with minority-friendly presumptions, electoral supermajority requirements for family law changes) without the collateral cost of enabling intra-community inequality?',
    'Comparative legal analysis: how do other multi-religious federations protect minority autonomy? Do those mechanisms provide equivalent protection with lower collateral cost? Thought-experiments: what would happen if the UCC were enacted with explicit carve-outs for minority communities, or with supermajority requirements?',
    'If equivalently protective alternatives exist with lower cost to intra-community subordinated groups, the constraint fails a proportionality test and the reading''s justification weakens — it becomes harder to defend the paralysis as necessary for stability. If no such alternatives are available or have been tried and failed, the reading''s claim that the constraint is structurally necessary is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_mechanisms_for_minority_protection, conceptual, 'Whether consociational fragmentation is the only available mechanism for minority protection or whether lower-cost alternatives could achieve the same goal.').

omega_variable(
    reading_foreclosure_gender_rights,
    'Does the gender-rights reading''s claim that intra-community gender equality should override minority autonomy logically foreclose the federalist reading''s claim that majority override capacity should be constitutionally limited?',
    'Examine the core premises: the federalist reading asserts that majority rule is itself a tyranny to be guarded against (by entrenching minority veto). The gender-rights reading asserts that minority-internal subordination is itself a tyranny to be guarded against (by imposing constitutional equality floors regardless of autonomy claims). These are distinct tyrannies with different structural locations. Can a framework affirm both, or does affirming one require denying the other?',
    'If the readings logically foreclose each other (one''s core premise contradicts the other''s), the relation is forecloses; if both can be held within a framework that prioritizes some tyrannies over others, the relation is coexists_with. The answer determines whether the federalist reading is genuinely live or has been logically displaced by gender-rights reasoning.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_gender_rights, conceptual, 'Whether the federalist and gender-rights readings are logically incompatible or can coexist in a single framework.').

omega_variable(
    community_leader_capture_ambiguity,
    'Do the religious community leaders who enforce personal law genuinely represent the interests of the communities they claim to serve, or have they captured the autonomy claim to entrench their own power over women and dissidents?',
    'Intra-community survey data on whether women and minorities within communities support the personal law system or are coerced into compliance. Analysis of exits: do people who leave the community-enforced law system report freedom or coercion? Historical analysis of how personal law provisions have evolved: have they liberalized to track community preferences, or have they remained static to entrench authority?',
    'If community leaders genuinely represent and are held accountable by their communities, the federalist reading''s claim that the constraint protects minority autonomy is strengthened. If leaders have captured autonomy language to entrench their power over dissidents (especially women), the constraint becomes a tool of intra-community domination wearing the mask of minority protection — it becomes snare-adjacent or tangled rope rather than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_leader_capture_ambiguity, empirical, 'Whether the constraint''s beneficiaries (community leaders) genuinely represent their communities or have captured the autonomy framework to entrench their own power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__federalist_millet_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(marr_tr_t15, marriage_authority__federalist_millet_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__federalist_millet_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(marr_tr_t45, marriage_authority__federalist_millet_reading, theater_ratio, 45, 0.2).
narrative_ontology:measurement(marr_tr_t60, marriage_authority__federalist_millet_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(marr_tr_t75, marriage_authority__federalist_millet_reading, theater_ratio, 75, 0.22).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__federalist_millet_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(marr_be_t15, marriage_authority__federalist_millet_reading, base_extractiveness, 15, 0.22).
narrative_ontology:measurement(marr_be_t30, marriage_authority__federalist_millet_reading, base_extractiveness, 30, 0.25).
narrative_ontology:measurement(marr_be_t45, marriage_authority__federalist_millet_reading, base_extractiveness, 45, 0.27).
narrative_ontology:measurement(marr_be_t60, marriage_authority__federalist_millet_reading, base_extractiveness, 60, 0.28).
narrative_ontology:measurement(marr_be_t75, marriage_authority__federalist_millet_reading, base_extractiveness, 75, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__federalist_millet_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(marr_su_t15, marriage_authority__federalist_millet_reading, suppression_requirement, 15, 0.1).
narrative_ontology:measurement(marr_su_t30, marriage_authority__federalist_millet_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement(marr_su_t45, marriage_authority__federalist_millet_reading, suppression_requirement, 45, 0.14).
narrative_ontology:measurement(marr_su_t60, marriage_authority__federalist_millet_reading, suppression_requirement, 60, 0.15).
narrative_ontology:measurement(marr_su_t75, marriage_authority__federalist_millet_reading, suppression_requirement, 75, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority__federalist_millet_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% The marriage_authority kernel decomposes into five distinct constraint readings. The federalist_millet_reading emphasizes consociational veto and legislative paralysis as features (stability, minority protection) rather than bugs. It structurally overlaps with the communal_autonomy_reading (both protect minority authority) but differs in the framing: autonomy rooted in tradition (communal reading) vs. autonomy as constitutional strategy against majority domination (federalist reading). The ε-invariance principle applies: the same legal architecture (personal law pluralism) yields different ε values under each reading because each reading foregrounds different aspects and different beneficiary/cost structures. Each reading is a separate story with its own claimed_type and metrics. They are linked via this network.affects_constraints array so the corpus can track how different readings of the same kernel produce different constraints and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__federalist_millet_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
