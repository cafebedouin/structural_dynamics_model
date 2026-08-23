% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Consociational Fragmentation of Marriage Authority (Federalist-Millet Reading)
 *   domain: constitutional/legal_pluralism/comparative_family_law
 *
 * SUMMARY:
 *   In millet-style legal pluralism, marriage and divorce authority is
 *   deliberately kept fragmented among recognized religious communities
 *   rather than consolidated under a common civil code. This story
 *   instantiates the federalist_millet reading of the marriage_authority
 *   kernel: fragmentation is not residue awaiting cleanup but the intended
 *   constitutional design — a consociational device that denies any majority,
 *   parliamentary or demographic, the power to legislate another community's
 *   family life. The ε referent is the standing arrangement under contest —
 *   the fragmented personal-law regime as it actually operates — assessed by
 *   this reading's own lights; the reading's endorsed alternative plays no
 *   role in the value. Per the claim/metric independence rule,
 *   claimed_type=rope states this reading's structural belief while the
 *   authored metrics record the arrangement's actual operation, including its
 *   victims; divergence between the claim and any per-seat computation is
 *   measurement, not error. The sibling readings (communal_autonomy,
 *   secularist, gender_rights, judicial_harmonization) instantiate other
 *   constraints over the same referent and are linked as a family; they are
 *   not described inside this one. KEY AGENTS (by structural relationship): -
 *   minority_communities: primary beneficiary (organized/constrained,
 *   generational) — hold communal family-law autonomy as the compact's core
 *   protection - communal_religious_leaderships: administering agenda-setter
 *   and receipt-seat (organized/identity_locked) — run the communal marriage
 *   machinery their office depends on - majority_party_elites: brokering
 *   agenda-setter (institutional/arbitrage) — concede autonomy, collect
 *   electoral peace - communal_rank_and_file_members: dual-positioned
 *   member-beneficiaries (moderate/identity_locked) — prefer communal law,
 *   live under leadership adjudication - women_under_unreformed_personal_law:
 *   primary payer (powerless/trapped) — bear unreformed communal terms with
 *   no neutral forum - interfaith_couples: payer (moderate/constrained) — no
 *   workable marriage channel across communal lines -
 *   secular_individuals_within_communities: payer (moderate/constrained) —
 *   want the civil option the compact rules out - constitutional_judiciary:
 *   observer (institutional/analytical) — lays incremental constitutional
 *   floors without legislating a code - ucc_advocacy_movements: excluded
 *   voice (organized/constrained) — campaigns outside the bargaining table
 *
 * KEY AGENTS:
 *   - minority_communities: primary beneficiary (organized/constrained) — communal family-law autonomy as protection against majoritarian codification
 *   - communal_religious_leaderships: agenda-setter and principal receipt-seat (organized/identity_locked) — jurisdiction, fees, and standing flow through the office the compact preserves
 *   - majority_party_elites: agenda-setter (institutional/arbitrage) — broker the veto that keeps uniform-code bills off the floor, collect communal electoral support
 *   - communal_rank_and_file_members: beneficiary with payer underside (moderate/identity_locked) — protection received, leadership authority endured
 *   - women_under_unreformed_personal_law: primary payer (powerless/trapped) — bear the compact's concentrated costs
 *   - interfaith_couples: payer (moderate/constrained) — the arrangement's boundary-maintenance function lands on them directly
 *   - secular_individuals_within_communities: payer (moderate/constrained) — the foreclosed civil option is their specific grievance
 *   - constitutional_judiciary: observer (institutional/analytical) — incremental floor-laying absorbs pressure the compact cannot
 *   - ucc_advocacy_movements: excluded (organized/constrained) — would veto the compact's terms but was never seated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.27).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.52).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.27).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Consociational Fragmentation of Marriage Authority (Federalist-Millet Reading)").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "constitutional/legal_pluralism/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__federalist_millet_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, '31c5fd5a-2656-4610-a7c1-ce8db38963c0').
narrative_ontology:cs_kernel_codification('31c5fd5a-2656-4610-a7c1-ce8db38963c0', formalized).
narrative_ontology:cs_authority_grounding('31c5fd5a-2656-4610-a7c1-ce8db38963c0', lineage).
narrative_ontology:cs_interpretation_layer_present('31c5fd5a-2656-4610-a7c1-ce8db38963c0').
narrative_ontology:cs_reading_relation('31c5fd5a-2656-4610-a7c1-ce8db38963c0', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('31c5fd5a-2656-4610-a7c1-ce8db38963c0', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('31c5fd5a-2656-4610-a7c1-ce8db38963c0', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('31c5fd5a-2656-4610-a7c1-ce8db38963c0', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('31c5fd5a-2656-4610-a7c1-ce8db38963c0', foundational, fragmentation_as_permanent_anti_majoritarian_design).
narrative_ontology:cs_axiom_status(fragmentation_as_permanent_anti_majoritarian_design, holdable).
narrative_ontology:cs_axiom_grounding('31c5fd5a-2656-4610-a7c1-ce8db38963c0', fragmentation_as_permanent_anti_majoritarian_design, instrumental).
narrative_ontology:cs_axiom('31c5fd5a-2656-4610-a7c1-ce8db38963c0', secondary, legislative_deadlock_is_protective_not_defective).
narrative_ontology:cs_axiom_status(legislative_deadlock_is_protective_not_defective, holdable).
narrative_ontology:cs_axiom_grounding('31c5fd5a-2656-4610-a7c1-ce8db38963c0', legislative_deadlock_is_protective_not_defective, instrumental).
narrative_ontology:cs_reference_frame('31c5fd5a-2656-4610-a7c1-ce8db38963c0', founding_consociational_compact).
narrative_ontology:cs_drift_state('31c5fd5a-2656-4610-a7c1-ce8db38963c0', contemporary_ucc_agitation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('31c5fd5a-2656-4610-a7c1-ce8db38963c0', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, minority_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, communal_religious_leaderships).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, majority_party_elites).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, communal_rank_and_file_members).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, women_under_unreformed_personal_law).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, interfaith_couples).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, secular_individuals_within_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, communal_rank_and_file_members).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, majoritarian_tyranny_thesis).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, consociational_stability_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, segmental_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religiously defined communities living as demographic minorities within a single constitutional state. They conduct marriages, divorces, and inheritance under their own recognized communal law rather than a common civil code. The guarantee that the majority cannot legislate their family life is the core protection the arrangement secures for them; the price is that their internal dissenters have no neutral forum to appeal to. Leaving would mean assimilation into majority law or emigration, and most members treat communal family law as inseparable from communal survival.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, minority_communities, beneficiary,
    organized, generational, constrained, national).

% Clerical bodies and customary councils that operate the communal marriage machinery: registering unions, adjudicating separations, setting maintenance terms, and disciplining deviation. Their jurisdiction exists only because the state declines to legislate a common family code; a uniform code would abolish their office. They bargain collectively with governing parties, delivering communal political support in exchange for renewed guarantees of autonomy. Office, income, standing, and adjudicative fees all flow to them through the arrangement they administer.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, communal_religious_leaderships, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__federalist_millet_reading, communal_religious_leaderships, beneficiary).

% Governing-party politicians who broker and renew the compact. They concede family-law autonomy to minority communities in exchange for electoral support and the absence of sectarian conflict on their watch, and they hold the legislative veto that keeps uniform-code bills from reaching a vote. Because their advantage comes from brokering rather than from any particular substantive outcome, they can switch platforms and champion a uniform code when the electoral calculus shifts.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, majority_party_elites, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__federalist_millet_reading, majority_party_elites, beneficiary).

% Ordinary members of minority communities who marry, divorce, and inherit under their communal law and largely prefer it to majority-imposed uniformity. They receive the arrangement's protection and also live under their leaderships' adjudicative authority; where personal preference diverges from communal norms, they find there is no forum between the two. Membership is bound up with family and identity, so exit is rare even where private dissatisfaction is real.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, communal_rank_and_file_members, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__federalist_millet_reading, communal_rank_and_file_members, payer).

% Women married under communal codes left unreformed while the majority's law modernized. Divorce, maintenance, polygamy, and custody rules bind them on terms the surrounding constitutional order officially disavows, yet no neutral civil forum is open to them inside the system; their recourse runs through their own community's institutions or through litigation aimed at the courts. Individual exit means losing family, community standing, and often custody of children.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, women_under_unreformed_personal_law, payer,
    powerless, immediate, trapped, national).

% Couples marrying across communal lines. Most millet-style regimes offer them no straightforward marriage channel: confessional registries do not recognize mixed unions, and the civil alternative, where one exists at all, is hedged with waiting periods, procedural burdens, and loss of communal inheritance rights. Many solemnize abroad or convert under the pressure of paperwork rather than conviction.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, interfaith_couples, payer,
    moderate, biographical, constrained, national).

% Community members who do not want religious adjudication of their family life but remain counted as community members for marriage purposes. They carry the communal law's obligations without sharing its convictions and cannot register a purely civil marriage in most such regimes. The common civil option they want is precisely what the compact rules out.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, secular_individuals_within_communities, payer,
    moderate, biographical, constrained, national).

% The apex courts that hear challenges from people the compact does not serve. Case by case they lay narrow constitutional floors under communal practice — maintenance guarantees, bans on instantaneous divorce forms — without legislating a common code, absorbing pressure that would otherwise either break the compact outright or leave it wholly untouched.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, constitutional_judiciary, observer,
    institutional, generational, analytical, national).

% Secularist and feminist campaigns pressing for a uniform civil code and for intra-communal reform. They organize, litigate, and contest elections, but hold no seat in the elite bargaining process through which family-law settlements are actually made; their influence arrives only when a crisis lets their issue attach itself to a majority-coalition agenda.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, ucc_advocacy_movements, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__federalist_millet_reading, communal_religious_leaderships).
narrative_ontology:fixing_cost_class(marriage_authority__federalist_millet_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds a religiously plural society inside one constitutional order by guaranteeing each community's family-law autonomy: it removes the majoritarian-homogenization threat that would otherwise drive minorities toward separatism or unrecognized parallel solemnization, and it provides predictable communal adjudication where a common code could not quickly earn trust.
% TRANSFER_FUNCTION: Moves adjudicative authority over marriage, divorce, and marital property from the state's common law to communal institutions; moves communal political support to brokerage parties in exchange for renewed autonomy guarantees; concentrates the costs of nonconforming preference — unequal divorce and maintenance terms, unavailable mixed marriage, no civil exit — on individuals inside each community.
% ABSENT_VOICES: Women under unreformed communal law, interfaith couples, and secular-minded community members have no seat in the elite bargaining process where family-law settlements are made; their objections surface only through courts, street mobilization, or attachment to majority-coalition agendas. The compact's unanimity is partly an artifact of who sat at the table at founding and who has been kept at it since.
% DISAPPEARANCE_RATIONALE: Overnight replacement by a common civil code would be experienced by minority communities as majoritarian conquest of their most intimate law: mass noncompliance, religious solemnization driven underground, communal parties breaking with the state, and — where the compact is woven into wider power-sharing such as confessional parliaments or reserved offices — the broader constitutional settlement shaking with it. Brokerage party systems organized around communal blocs would reorganize around the new cleavage. The current allocation of marriage authority is not self-enforcing without the compact.
% FOUNDING_PROBLEM: At constitution-making, the problem was integrating communities with entrenched, religiously authorized family-law traditions into a single democratic state without triggering mass defection, sectarian violence, or permanent communal privilege: majorities feared frozen privilege, minorities feared demographic domination of their intimate law. The compact traded a common civil code for communal autonomy guarantees and mutual elite vetoes.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative-law scholars corroborate the founding trade-off from constituent-assembly and mandate-era records: minority delegations demanded personal-law guarantees and majority negotiators conceded to prevent schism; the consociational literature attests segmental autonomy as designed conflict management — both sources sit outside the benefiting parties. Persistence of the problem is disputed: minority leaderships attest it remains live, while women's organizations and uniform-code advocates attest the acute integration phase closed decades ago and the arrangement now manufactures the division it administers. That dispute is why the status is recorded as contested rather than live.
narrative_ontology:disappearance_verdict(marriage_authority__federalist_millet_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__federalist_millet_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__federalist_millet_reading, 0.27, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored low (0.27, endpoint of the series) because the reading weighs the compact's protective function heavily: minorities demonstrably prefer communal law to majoritarian codification in revealed-choice moments, and the costs that remain are real but concentrated on insider seats the bargain never seated. Suppression (0.52) is a raw, unscaled structural property — the civil alternative is foreclosed by veto politics and, in many jurisdictions, by statutory absence; it is not scaled by power or scope, and the engine owns any such arithmetic downstream. Theater is low-moderate (0.22): adjudication is functional; the performative share peaks around crisis management (consultation theater before the 1986 reversal, symbolic committee revivals circa 2013) and drops when concrete statutes displace ritual (2019). Accessibility_collapse (0.42) reflects partly collapsed alternatives: emigration and conversion exist but are heavy; the domestic civil route is politically foreclosed. Resistance (0.55) is real and continuously absorbed rather than crushed — women's litigation, secularist campaigning, internal reform pressure. Enforcement is active (requires_active_enforcement: true) but political rather than carceral: whips, patronage, and elite renewal. There is deliberately no sunset clause — consociational designs decline transitional framing; that absence is a structural signature distinguishing this arrangement from scaffold-type constraints and is the exact premise the secularist sibling reading rejects.
 *   
 *   Temporal dynamics: the series runs on one shared ten-point grid (all three tracked metrics authored at every point — the alignment rule). The pattern is cyclical with a mild pre-2019 ratchet: crisis (a court ruling threatening the bargain) -> override/backlash -> elite re-consolidation -> calm, visible at 1986 (peak suppression 0.58, peak extractiveness 0.31) and in modified form at 2019 (selective breach: one abusive practice criminalized, extractiveness dips). The oscillation is partly an intermittent-reinforcement mechanism — each re-consolidation re-entrenches leadership authority and slightly raises the insider cost floor — though the 2019 breach shows the cycle is not fully captive. Scalars reflect the late-interval state. Boltzmann coordination type is identity_coordination: the compact's primary function is boundary maintenance and membership definition through family law; the known gaming risk of identity cover stories is handled by the minority_preference_vs_elite_articulation omega rather than by a floor override.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the payer seats compute differently from the same structure. From the leaderships' position the compact is the peace they broker and the office it sustains; from majority elites' position it is cheap stability purchased with concessions they can withdraw; from the payer seats it is a ceiling over their most intimate decisions with the exit doors bricked up. Rank-and-file members sit genuinely between: protection received, adjudicative authority endured. There is a second, kernel-level gap: sibling readings author different story-level epsilon over the same referent — the gender_rights and secularist readings weight the payer seats' experience far more heavily. The engine computes per-seat classifications from the structural data; the reading_index_epsilon_divergence omega tracks the cross-reading divergence, which no single story can adjudicate.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for minority_communities and communal_rank_and_file_members (protection received), communal_religious_leaderships (office and fees), and majority_party_elites (electoral peace). Victim declarations drive high directionality for the three payer seats, amplified by exit atoms: women_under_unreformed_personal_law sit nearest the full-target end (trapped exit, powerless, immediate horizon); interfaith_couples and secular_individuals_within_communities are high-d with constrained exit. The constitutional_judiciary carries the analytical atom and exerts no directional pull; ucc_advocacy_movements hold the excluded role — outside the derivation, voiced instead through absent_voices. Receipt concentration: the compact's tangible transfers (jurisdiction, adjudicative fees, standing) accrue to the communal leadership seat, which is why gain_flow names it rather than reporting diffuse. No directionality_overrides were authored: the beneficiary/victim declarations plus exit atoms already place every seat correctly, and the derivation chain needs no correction.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy signature is present: theater is low, adjudicative function is operative, and the founding problem's status is contested rather than dead — the compact has not outlived a vanished function so much as become the object of a live dispute over which function counts. Fixing is prohibitive for the only actor positioned to fix it (the national government): imposing a common code would cost electoral wipeout in minority constituencies, probable unrest, and rupture of any wider power-sharing woven into the compact — far exceeding the benefit to the fixer. The rope claim guards against mislabeling the compact as pure extraction: its protective coordination is real and demonstrated whenever majorities have tried to codify. The victim declarations guard against rope-complacency: costs concentrate on identifiable insider seats with no forum. Scaffold is inapplicable (designers declined sunset framing; the transitional reading of this arrangement belongs to the secularist sibling story, which is a different constraint). Piton is inapplicable: administration tracks function, not performance, and no seat profits so little that maintenance has gone theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_index_epsilon_divergence,
    'This story instantiates the federalist_millet reading and authors epsilon=0.27 for the standing fragmented-marriage-authority arrangement; sibling readings author materially different values over the same referent — where exactly is the disagreement located?',
    'Side-by-side comparison of the sibling stories'' epsilon values, victim sets, and claimed types, classifying divergence as weighting-of-shared-facts (conceptual) versus conflicting factual description (empirical).',
    'Weighting-only divergence confirms the readings coexist as evaluative frames mediated by the engine''s per-seat computations; factual divergence flags one or more stories as misdescribing the arrangement and identifies which.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_index_epsilon_divergence, conceptual, 'Reading-indexed epsilon divergence across the marriage_authority kernel''s sibling readings.').

omega_variable(
    minority_preference_vs_elite_articulation,
    'Do ordinary members of minority communities actually prefer communal family law to a rights-protective common code, or does elite articulation mask latent individual demand?',
    'Revealed-preference evidence: uptake of optional civil marriage where introduced, survey data inside communities on hypothetical civil options, migration of registrations to civil forums wherever legally possible.',
    'High latent demand would mean the beneficiary declaration overstates the coordination function, effective extraction on rank-and-file seats rises, and the arrangement shifts toward extraction riding identity cover; authentic preference supports the rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_preference_vs_elite_articulation, empirical, 'Whether communal-law preference is authentic or elite-manufactured (the identity_coordination gaming-risk check for this story).').

omega_variable(
    civil_option_absence_statutory_vs_social,
    'Is the unavailability of a neutral civil marriage channel primarily statutory (no enabling law exists) or practical-social (law exists but is unusable given cost and stigma)?',
    'Cross-jurisdiction comparison: jurisdictions with enabling statutes but low uptake versus jurisdictions without statutes; measure uptake differentials holding community composition constant.',
    'Statutory absence locates the foreclosure in the compact itself and supports the authored suppression level; a usable-but-unused statute relocates suppression to community-level enforcement and lowers the compact''s own suppression score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_option_absence_statutory_vs_social, empirical, 'Structural versus social mechanism behind civil-marriage foreclosure.').

omega_variable(
    legislative_paralysis_function,
    'Is repeated legislative deadlock on family law an engineered stability feature of the compact (this reading''s claim) or elite self-protection dressed in stability language?',
    'Compare deadlock incidence and veto-player behavior on family-law bills against matched control policy domains; trace which actors block, at whose request, and what they receive in return.',
    'An engineered-feature finding supports the rope claim and the anti-tyranny rationale; a self-protection finding recasts the paralysis as maintenance of leadership rents and pushes per-seat classifications toward extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_paralysis_function, empirical, 'Function of family-law legislative paralysis: stability mechanism or elite protection.').

omega_variable(
    secularist_terminal_status_contradiction,
    'This reading asserts fragmentation is permanent constitutional design while the secularist sibling asserts it is a transitional anomaly awaiting elimination — does that confirm no single constitutional framework can coherently hold both (the authored forecloses edge), or does a genuine staged-transition reading exist that would dissolve the contradiction?',
    'Analytical test: attempt to construct a stable intermediate framework (guaranteed interim pluralism with a credible, dated path to unity) and determine whether it constitutes a third reading rather than an unstable blend of two.',
    'Confirmation validates the forecloses relation to the secularist sibling; a viable staged reading would downgrade the edge to coexists_with and split the kernel further.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secularist_terminal_status_contradiction, conceptual, 'Logical status of the terminal-versus-transitional contradiction between this reading and the secularist sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1948, marriage_authority__federalist_millet_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement_basis(marr_tr_t1948, observed).
narrative_ontology:measurement(marr_tr_t1956, marriage_authority__federalist_millet_reading, theater_ratio, 1956, 0.17).
narrative_ontology:measurement_basis(marr_tr_t1956, observed).
narrative_ontology:measurement(marr_tr_t1968, marriage_authority__federalist_millet_reading, theater_ratio, 1968, 0.19).
narrative_ontology:measurement_basis(marr_tr_t1968, observed).
narrative_ontology:measurement(marr_tr_t1979, marriage_authority__federalist_millet_reading, theater_ratio, 1979, 0.21).
narrative_ontology:measurement_basis(marr_tr_t1979, observed).
narrative_ontology:measurement(marr_tr_t1986, marriage_authority__federalist_millet_reading, theater_ratio, 1986, 0.24).
narrative_ontology:measurement_basis(marr_tr_t1986, observed).
narrative_ontology:measurement(marr_tr_t1995, marriage_authority__federalist_millet_reading, theater_ratio, 1995, 0.23).
narrative_ontology:measurement_basis(marr_tr_t1995, observed).
narrative_ontology:measurement(marr_tr_t2004, marriage_authority__federalist_millet_reading, theater_ratio, 2004, 0.22).
narrative_ontology:measurement_basis(marr_tr_t2004, observed).
narrative_ontology:measurement(marr_tr_t2013, marriage_authority__federalist_millet_reading, theater_ratio, 2013, 0.24).
narrative_ontology:measurement_basis(marr_tr_t2013, observed).
narrative_ontology:measurement(marr_tr_t2019, marriage_authority__federalist_millet_reading, theater_ratio, 2019, 0.2).
narrative_ontology:measurement_basis(marr_tr_t2019, observed).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority__federalist_millet_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(marr_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1948, marriage_authority__federalist_millet_reading, base_extractiveness, 1948, 0.2).
narrative_ontology:measurement_basis(marr_be_t1948, observed).
narrative_ontology:measurement(marr_be_t1956, marriage_authority__federalist_millet_reading, base_extractiveness, 1956, 0.24).
narrative_ontology:measurement_basis(marr_be_t1956, observed).
narrative_ontology:measurement(marr_be_t1968, marriage_authority__federalist_millet_reading, base_extractiveness, 1968, 0.26).
narrative_ontology:measurement_basis(marr_be_t1968, observed).
narrative_ontology:measurement(marr_be_t1979, marriage_authority__federalist_millet_reading, base_extractiveness, 1979, 0.28).
narrative_ontology:measurement_basis(marr_be_t1979, observed).
narrative_ontology:measurement(marr_be_t1986, marriage_authority__federalist_millet_reading, base_extractiveness, 1986, 0.31).
narrative_ontology:measurement_basis(marr_be_t1986, observed).
narrative_ontology:measurement(marr_be_t1995, marriage_authority__federalist_millet_reading, base_extractiveness, 1995, 0.29).
narrative_ontology:measurement_basis(marr_be_t1995, observed).
narrative_ontology:measurement(marr_be_t2004, marriage_authority__federalist_millet_reading, base_extractiveness, 2004, 0.28).
narrative_ontology:measurement_basis(marr_be_t2004, observed).
narrative_ontology:measurement(marr_be_t2013, marriage_authority__federalist_millet_reading, base_extractiveness, 2013, 0.29).
narrative_ontology:measurement_basis(marr_be_t2013, observed).
narrative_ontology:measurement(marr_be_t2019, marriage_authority__federalist_millet_reading, base_extractiveness, 2019, 0.26).
narrative_ontology:measurement_basis(marr_be_t2019, observed).
narrative_ontology:measurement(marr_be_t2024, marriage_authority__federalist_millet_reading, base_extractiveness, 2024, 0.27).
narrative_ontology:measurement_basis(marr_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1948, marriage_authority__federalist_millet_reading, suppression_requirement, 1948, 0.35).
narrative_ontology:measurement_basis(marr_su_t1948, observed).
narrative_ontology:measurement(marr_su_t1956, marriage_authority__federalist_millet_reading, suppression_requirement, 1956, 0.4).
narrative_ontology:measurement_basis(marr_su_t1956, observed).
narrative_ontology:measurement(marr_su_t1968, marriage_authority__federalist_millet_reading, suppression_requirement, 1968, 0.44).
narrative_ontology:measurement_basis(marr_su_t1968, observed).
narrative_ontology:measurement(marr_su_t1979, marriage_authority__federalist_millet_reading, suppression_requirement, 1979, 0.5).
narrative_ontology:measurement_basis(marr_su_t1979, observed).
narrative_ontology:measurement(marr_su_t1986, marriage_authority__federalist_millet_reading, suppression_requirement, 1986, 0.58).
narrative_ontology:measurement_basis(marr_su_t1986, observed).
narrative_ontology:measurement(marr_su_t1995, marriage_authority__federalist_millet_reading, suppression_requirement, 1995, 0.54).
narrative_ontology:measurement_basis(marr_su_t1995, observed).
narrative_ontology:measurement(marr_su_t2004, marriage_authority__federalist_millet_reading, suppression_requirement, 2004, 0.5).
narrative_ontology:measurement_basis(marr_su_t2004, observed).
narrative_ontology:measurement(marr_su_t2013, marriage_authority__federalist_millet_reading, suppression_requirement, 2013, 0.49).
narrative_ontology:measurement_basis(marr_su_t2013, observed).
narrative_ontology:measurement(marr_su_t2019, marriage_authority__federalist_millet_reading, suppression_requirement, 2019, 0.47).
narrative_ontology:measurement_basis(marr_su_t2019, observed).
narrative_ontology:measurement(marr_su_t2024, marriage_authority__federalist_millet_reading, suppression_requirement, 2024, 0.52).
narrative_ontology:measurement_basis(marr_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'personal law / plural marriage authority' covers at least five structurally distinct claims and is decomposed into five linked stories sharing one referent — the fragmented arrangement — while differing in reading-indexed epsilon, beneficiary/victim emphasis, and claimed type. Ordering: communal_autonomy_reading is the upstream tradition-grounded story (low epsilon, established) whose guarantees the federalist bargain repackages; this federalist_millet story (rope claim, low epsilon, declared victims) legitimizes the veto architecture that judicial_harmonization_reading exploits and gender_rights_reading contests; secularist_reading is the elimination pole whose terminal-status premise directly contradicts this reading's. Every member links to the others via network.affects_constraints; no story hedges epsilon across readings, and the divergence in authored values across the family is itself the measurement the family exists to take.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
