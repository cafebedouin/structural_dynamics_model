% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__individual_right_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: second_amendment_arms_right__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading (Post-Heller Settlement)
 *   domain: constitutional law/political philosophy/legal interpretation
 *
 * SUMMARY:
 *   The standing arrangement under contest is the post-Heller constitutional
 *   settlement in United States firearms law: a federally enforceable
 *   guarantee that individual persons may keep and bear arms, grounded in the
 *   claim that this liberty predates government and therefore cannot be
 *   abrogated by ordinary legislation. This file instantiates ONE reading of
 *   the Second Amendment kernel — the individual_right_reading; the
 *   collective_right_reading and civic_republican_reading are separate
 *   constraint stories with their own beneficiary structures and their own
 *   epsilon values, linked through the network edges. The arrangement
 *   operates as follows: courts strike down or deter federal (and,
 *   post-incorporation, state) prohibition measures; a commercial and
 *   advocacy ecosystem organizes around the guaranteed market; and the
 *   prevalence costs of widespread civilian armament fall substantially on
 *   communities and households that did not choose them and lack a
 *   compensating channel. Epsilon's referent is this standing arrangement,
 *   assessed by the individual-right reading's own lights: the reading
 *   regards the guarantee as protective rather than taking, while conceding
 *   that prevalence costs land on third parties and declining to treat
 *   prohibition as a legitimate remedy — which is why epsilon sits mid-range
 *   rather than near zero. KEY AGENTS (by structural relationship): see
 *   key_agents; the protected class and its commercial ecosystem hold the
 *   beneficiary side, the regulatory apparatus and violence-exposed publics
 *   hold the cost-bearing side, and the judiciary administers the perimeter.
 *
 * KEY AGENTS:
 *   - - individual_gun_owners: Primary beneficiary (organized/constrained) — the protected class; hold secured access to arms; receive the guarantee's liberty value
 *   - - firearms_industry: Concentrated commercial beneficiary (powerful/arbitrage) — sells into the guaranteed market; captures the arrangement's material rents
 *   - - gun_rights_advocacy_organizations: Secondary beneficiary (organized/mobile) — converts defense of the guarantee into dues, relevance, and fundraising
 *   - - federal_regulatory_authorities: Primary constrained party (institutional/trapped) — prohibition and broad-restriction instruments foreclosed or made litigation-prohibitive
 *   - - state_regulatory_authorities: Constrained party (institutional/constrained) — retains narrower regulatory space post-incorporation; prohibition ambitions bounded
 *   - - urban_communities_exposed_to_gunfire: Cost-bearing public (powerless/trapped) — bears elevated violence exposure tied to civilian armament prevalence
 *   - - domestic_violence_risk_households: Cost-bearing households (powerless/trapped) — intimate-partner lethality scales with firearm access
 *   - - federal_courts: Agenda-setter (institutional/analytical) — adjudicates the guarantee's scope; its interpretations define the shield's perimeter
 *   - - public_health_research_community: Excluded voice (moderate/constrained) — evidence producer kept outside the interpretive coalition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.58).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.64).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Second Amendment Individual Right Reading (Post-Heller Settlement)").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "constitutional law/political philosophy/legal interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, 'addc65ab-4b71-42d3-8138-205c6d533626').
narrative_ontology:cs_kernel_codification('addc65ab-4b71-42d3-8138-205c6d533626', fixed_text).
narrative_ontology:cs_authority_grounding('addc65ab-4b71-42d3-8138-205c6d533626', lineage).
narrative_ontology:cs_interpretation_layer_present('addc65ab-4b71-42d3-8138-205c6d533626').
narrative_ontology:cs_reading_relation('addc65ab-4b71-42d3-8138-205c6d533626', second_amendment_arms_right__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('addc65ab-4b71-42d3-8138-205c6d533626', second_amendment_arms_right__civic_republican_reading, influences).
narrative_ontology:cs_axiom('addc65ab-4b71-42d3-8138-205c6d533626', foundational, arms_bearing_pre_existing_individual_liberty).
narrative_ontology:cs_axiom_status(arms_bearing_pre_existing_individual_liberty, holdable).
narrative_ontology:cs_axiom_grounding('addc65ab-4b71-42d3-8138-205c6d533626', arms_bearing_pre_existing_individual_liberty, deontological).
narrative_ontology:cs_axiom('addc65ab-4b71-42d3-8138-205c6d533626', secondary, federal_infringement_of_arms_possession_illegitimate).
narrative_ontology:cs_axiom_status(federal_infringement_of_arms_possession_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('addc65ab-4b71-42d3-8138-205c6d533626', federal_infringement_of_arms_possession_illegitimate, deontological).
narrative_ontology:cs_reference_frame('addc65ab-4b71-42d3-8138-205c6d533626', pre_existing_individual_liberty_guarantee).
narrative_ontology:cs_drift_state('addc65ab-4b71-42d3-8138-205c6d533626', contemporary_post_bruen, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('addc65ab-4b71-42d3-8138-205c6d533626', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, urban_communities_exposed_to_gunfire).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, domestic_violence_risk_households).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, federal_regulatory_authorities).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, state_regulatory_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Roughly a third of U.S. households hold firearms for self-defense, sport, and collection. The constitutional guarantee secures their access against federal prohibition; their practical exit from the arrangement (disarming or emigrating) is costly and beside the point, since they are the class the guarantee protects.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, constrained, national).

% Manufacturers, importers, and retailers sell into a civilian market whose legal foundation the guarantee secures. Revenue tracks the breadth of the protected policy space; when one product channel tightens, firms pivot calibers, accessories, or export markets.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, firearms_industry, beneficiary,
    powerful, immediate, arbitrage, global).

% Membership organizations convert defense of the guarantee into dues, relevance, and fundraising energy. Their fortunes rise with perceived threats to the arrangement, so litigation waves and legislative scares are recruitment events rather than pure costs.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_rights_advocacy_organizations, beneficiary,
    organized, biographical, mobile, national).

% Congress, DOJ, and ATF hold prohibition and broad-restriction instruments that the guarantee forecloses or renders litigation-prohibitive. They must draft around text-history-tradition review, and they cannot resign from the governing duty the guarantee bounds.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_regulatory_authorities, payer,
    institutional, generational, trapped, national).

% Since incorporation, state assault-weapon and handgun bans face the same shield as federal ones. States retain narrower space (licensing, waiting periods, sensitive-place rules) but their prohibition ambitions are bounded, and they bear the litigation cost of testing the perimeter.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, state_regulatory_authorities, payer,
    institutional, generational, constrained, regional).

% Neighborhoods with elevated homicide and injury exposure bear the prevalence costs of widespread civilian armament. Their preferred remedies run through the instruments the guarantee constrains, and they cannot exit the national firearms market's externalities by moving.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, urban_communities_exposed_to_gunfire, payer,
    powerless, biographical, trapped, local).

% Intimate-partner homicide risk scales with firearm access in the home. Protective instruments such as surrender mandates and extreme-risk protection orders sit inside the guarantee's perimeter and are narrowed where the perimeter is drawn expansively.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, domestic_violence_risk_households, payer,
    powerless, biographical, trapped, local).

% The judiciary adjudicates what the guarantee covers. Its interpretations define the shield's perimeter, absorb drift without textual revision, and determine which regulatory instruments survive contact with review.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Injury epidemiologists and health-services researchers produced the prevalence-cost evidence base until federal funding for gun-violence research was curtailed in the late 1990s. They would press that evidence into policy design but sit largely outside the interpretive coalition that produced the settlement.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, public_health_research_community, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__individual_right_reading, firearms_industry).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, judicially enforceable guarantee that individuals may possess arms, solving a credible-commitment problem: citizens' defensive capacity cannot be revoked by shifting federal majorities, and the boundary between lawful possession and infringing regulation is standardized rather than renegotiated each session.
% TRANSFER_FUNCTION: Moves regulatory discretion over firearms from federal (and incorporated state) governments to individual owners and the commercial ecosystem serving them; moves the prevalence costs of civilian armament onto violence-exposed communities and households, uncompensated.
% ABSENT_VOICES: Public-health researchers, urban violence survivors, and prohibition-seeking municipalities were largely absent from the interpretive coalition that produced the individual-right settlement; their objection — that prevalence costs land on non-consenting third parties — enters chiefly through litigation briefs and amici, not through seats at the interpretive table.
% DISAPPEARANCE_RATIONALE: If the guarantee vanished overnight, federal and state prohibition instruments reopen immediately, the litigation economy around the perimeter collapses, industry market assumptions rearrange around regulatory risk, and the fifty-state patchwork would consolidate toward whichever regime each legislature prefers — the arrangement, not mere custom, is what holds the current allocation in place.
% FOUNDING_PROBLEM: Ratification-era fear that a standing federal army and federal disarmament authority would strip the citizenry (and with it the militia system) of arms; Anti-Federalists demanded an arms guarantee as a check on federal military power.
% FOUNDING_PROBLEM_CORROBORATION: That the founding problem existed is corroborated outside the beneficiary set by ratification-era records: state ratifying conventions' proposed amendments, Anti-Federalist pamphlets, and the militia politics of the 1780s. Whether it remains LIVE is attested almost exclusively from inside the beneficiary coalition (advocacy organizations citing confiscation risk); gun-policy historians writing outside that coalition treat the original problem as historical, and no neutral contemporary institution attests an imminent federal disarmament program. That asymmetry is itself signal.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_arms_right__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__individual_right_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_arms_right__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58 decomposes as: a genuine protective function delivered to the holder class (which pulls down), against an uncompensated third-party cost channel and a concentrated commercial rent stream (which push up). The reading itself concedes the externality channel exists — it disputes only the remedy — so a reading-honest epsilon cannot sit near zero. Suppression 0.64 reflects active foreclosure of the prohibition alternative: the guarantee persists through judicial enforcement, and the Bruen-era text-history-tradition review has struck broad regulatory categories, raising the enforcement intensity the arrangement requires. Theater 0.28: the doctrine is functionally operative (real strikes, real protections, real market effects) with a ceremonial-symbolic layer (militia-preamble rhetoric, symbolic legislation) that has grown slowly. Accessibility_collapse 0.45: the federal prohibition alternative has largely collapsed, but state regulatory space survives post-incorporation, so alternatives are narrowed rather than eliminated. Resistance 0.70: the arrangement meets sustained organized opposition — the gun-control movement, municipal and state litigation, and a hostile scholarly literature — and must be continuously defended. CLAIM/METRIC INDEPENDENCE: claimed_type tangled_rope is authored from structural analysis (a real rights-coordination function PLUS asymmetric cost incidence PLUS active enforcement); the metrics are authored descriptively of actual operation; the engine computes per-seat classifications from the structural data and any divergence between claim and computed type is the measurement, not an error. Temporal pattern: the series moves in litigation-milestone steps (1977 organizational realignment, 1986 and 1994 federal statutes, 2008 Heller, 2010 McDonald, 2022 Bruen) rather than smooth drift; separately, scare-driven demand cycles (post-election, post-tragedy purchasing surges) oscillate industry revenue within the trend — a cyclical layer documented here but expressed mostly in commercial flows rather than in the doctrine's structural metrics.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently, and the engine derives this from the structural data. From the protected class and its commercial ecosystem, the arrangement is a liberty guarantee they built political support for and defend; from the constrained regulatory seats, the same structure is a removal of policy instruments they cannot recover; from the violence-exposed seats, it is an uncompensated externality regime whose remedy is foreclosed. Same-power differentiation: federal and state regulators hold equal nominal institutional standing but different exits — the federal seat is trapped (it cannot shed the governing duty), the state seat merely constrained (it retains licensing and sensitive-place space) — so identical power atoms yield different directionalities. Coalition check: the powerless victim seats have repeatedly formed coalitions (municipal consortia, philanthropically funded advocacy) and remain outmatched, because the beneficiary coalition holds an incumbency advantage — the arrangement it defends is the status quo the courts enforce.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: individual_gun_owners sit far toward the subsidized end (the guarantee exists for them); firearms_industry sits lowest of all (direct receipt of the arrangement's material gains plus arbitrage-grade exit); advocacy organizations sit low but slightly above the industry (they harvest political rents that depend on continued contestation). Victim declarations drive high directionality: urban communities and domestic-violence-risk households are trapped targets (no exit from the externality, no remedy instrument); federal regulators are high-directionality despite institutional power because their exit is trapped — power does not buy exit from a duty the guarantee bounds; state regulators sit slightly lower than federal (retained regulatory space damps effective extraction). The judiciary is near-symmetric and analytical. No directionality_overrides are authored: the derivation chain from beneficiary/victim declarations plus exit options reproduces these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal disarmament of the citizenry and the militia system) is CONTESTED rather than dead: the original form — a federal program of general confiscation — has no contemporary institutional carrier, but the beneficiary coalition attests liveness via confiscation-risk claims, and the arrangement's protective function is real for holders even if the triggering threat is hypothetical. The mismatch consumer reads founding_problem_status x disappearance_verdict: contested + world_rearranges produces no zombie flag, correctly — the arrangement demonstrably holds allocations in place regardless of the founding problem's status. Function migration is visible but modest: the guarantee's operative center has shifted from militia-check to individual self-defense identity and commerce (tracked by the theater_ratio series, 0.14 to 0.28 — well short of piton range). Mandatrophy discipline here prevents two opposite mislabels: calling the arrangement a snare erases the genuine liberty function tens of millions exercise and defend; calling it a rope erases the uncompensated externality channel and the concentrated commercial rents that ride the same structure. Tangled_rope holds both halves, which is what the structural record shows.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pre_existence_vs_constructed_doctrine,
    'Is the right genuinely a pre-existing natural liberty (as this reading''s foundational axiom asserts), or a constructed constitutional settlement maintained by active enforcement?',
    'Comparative constitutional history and ratification-era scholarship distinguishing declared natural rights from rights constituted by enactment; tracing whether the guarantee''s operative content tracks any continuous pre-1791 practice or was assembled by twentieth-century advocacy and twenty-first-century case law.',
    'If constructed, the reading''s naturality claim fails and the arrangement stands as ordinary positive law — relevant to whether any naturality certification could ever attach, and to how much of the arrangement''s persistence rests on enforcement rather than acceptance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_existence_vs_constructed_doctrine, conceptual, 'Whether the reading''s pre-existence claim describes reality or is a legitimating frame over a constructed arrangement.').

omega_variable(
    kernel_bearer_disagreement_location,
    'Which structural element of the kernel do the sibling readings actually disagree on — the bearer of ''the people'', the pre-existence claim, or the operative purpose — and how would each sibling''s instantiation change this file''s beneficiary and victim sets?',
    'Side-by-side compilation of the three reading files: the collective_right_reading moves individual_gun_owners out of the beneficiary set and installs state_militia_authorities; the civic_republican_reading reframes beneficiaries as the armed civic body and changes the transfer function from private self-defense to civic capacity.',
    'Determines whether the three files form a genuine constraint family with divergent epsilon over one referent, or whether one reading''s structural description subsumes the others; routes the committer contest through the omega apparatus instead of contaminating this file''s classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_bearer_disagreement_location, conceptual, 'Location and structural consequences of the inter-reading disagreement over the Second Amendment kernel.').

omega_variable(
    externality_attribution_dispute,
    'Are the violence costs borne by exposed communities attributable to the arrangement itself (permissive baseline the guarantee locks in), or to criminal misuse that would persist under any lawful-possession regime?',
    'Right-to-carry and prohibition-natural-experiment econometrics (synthetic-control and panel studies of shall-issue adoption, may-issue repeal, and ban implementations), disaggregating lawful-prevalence effects from criminal-acquisition effects.',
    'If prevalence itself drives the cost curve, the extraction asymmetry is genuine and the tangled_rope structure holds; if costs are independent of lawful possession, the asymmetry thins and the arrangement shifts toward rope — the single largest lever on this file''s classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_attribution_dispute, empirical, 'Whether the third-party cost channel is caused by the arrangement or merely correlated with it.').

omega_variable(
    confiscation_threat_liveness,
    'Is the anti-disarmament protective function live (a realistic federal confiscation trajectory the guarantee forestalls) or has it become theatrical maintenance of a solved problem?',
    'Track federal legislative proposals for confiscatory programs, agency rulemaking trajectories, and court dockets over successive congresses; a persistent absence of viable confiscatory vehicles across decades indicates the protective function is decaying toward performance.',
    'If the threat is dead, the protective-function share of the arrangement shrinks, theater_ratio rises, and the file drifts toward piton on the inertia pathway; if live, the protective function justifies its share of the arrangement''s cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(confiscation_threat_liveness, empirical, 'Liveness of the founding-era disarmament threat that anchors the reading''s protective claim.').

omega_variable(
    incorporation_scope_boundary,
    'Does this reading bind only the federal government (as the kernel text''s ratification-era scope suggests) or also the states via Fourteenth-Amendment incorporation — and does the answer change the victim set?',
    'Track the case-law perimeter: whether incorporation (McDonald line) and its Bruen-era extension are treated as constitutive of this reading or as a separable overlay; compare state-regulator directionality under federal-only versus incorporated framings.',
    'Under a federal-only framing, state_regulatory_authorities exit the victim set and the arrangement''s scope narrows (lower effective extraction amplification); under full incorporation, the current four-seat victim set holds and scope is national.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incorporation_scope_boundary, conceptual, 'Boundary of the reading''s reach — federal-only versus incorporated — and its effect on the structural delta.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 1960, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1960, second_amendment_arms_right__individual_right_reading, theater_ratio, 1960, 0.14).
narrative_ontology:measurement(seco_tr_t1977, second_amendment_arms_right__individual_right_reading, theater_ratio, 1977, 0.16).
narrative_ontology:measurement(seco_tr_t1986, second_amendment_arms_right__individual_right_reading, theater_ratio, 1986, 0.18).
narrative_ontology:measurement(seco_tr_t1994, second_amendment_arms_right__individual_right_reading, theater_ratio, 1994, 0.21).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__individual_right_reading, theater_ratio, 2008, 0.24).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_arms_right__individual_right_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_arms_right__individual_right_reading, theater_ratio, 2022, 0.27).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_arms_right__individual_right_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(seco_be_t1960, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1960, 0.3).
narrative_ontology:measurement(seco_be_t1977, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1977, 0.33).
narrative_ontology:measurement(seco_be_t1986, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1986, 0.37).
narrative_ontology:measurement(seco_be_t1994, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1994, 0.43).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2008, 0.51).
narrative_ontology:measurement(seco_be_t2010, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2010, 0.54).
narrative_ontology:measurement(seco_be_t2022, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2022, 0.58).
narrative_ontology:measurement(seco_be_t2026, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1960, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1960, 0.18).
narrative_ontology:measurement(seco_su_t1977, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1977, 0.21).
narrative_ontology:measurement(seco_su_t1986, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1986, 0.26).
narrative_ontology:measurement(seco_su_t1994, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1994, 0.31).
narrative_ontology:measurement(seco_su_t2008, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2008, 0.48).
narrative_ontology:measurement(seco_su_t2010, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2010, 0.53).
narrative_ontology:measurement(seco_su_t2022, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2022, 0.63).
narrative_ontology:measurement(seco_su_t2026, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2026, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__civic_republican_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, plcaa_civil_liability_shield).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the Second Amendment kernel per the epsilon-invariance principle: the colloquial label 'the Second Amendment' conflates three structurally distinct claims about WHOSE right the text protects and WHY. This file (individual_right_reading) carries the individual-bearer structure with individual_gun_owners and firearms_industry in the beneficiary set; the collective_right_reading file carries the state-militia-bearer structure with a disjoint beneficiary set; the civic_republican_reading file carries the armed-civic-body structure. Each file authors its own epsilon over the SHARED referent (the standing U.S. firearms arrangement) through its own lights, so the three epsilons differ by construction. Upstream/downstream: this reading's post-2008 ascendancy created the structural conditions (text-history-tradition review, industry legal confidence) on which the PLCAA civil-liability-shield arrangement depends, hence the affects edge; the sibling edges carry the reading-relations declared in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
