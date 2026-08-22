% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__lapsed_alternative_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: market_as_natural_default__lapsed_alternative_reading
 *   human_readable: Market Default via Lapsed Alternatives (Lapsed-Memory Reading)
 *   domain: political economy/ideology studies/economic history
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel (see
 *   kernel_context). The standing arrangement under contest: market forms —
 *   privatized utilities, auctioned spectrum, voucher schemes, shareholder
 *   firms — operate as the unmarked, justification-free default of economic
 *   policy, while cooperative, commons-based, municipal, and mutual
 *   arrangements require special pleading whenever they surface. The
 *   lapsed_alternative_reading accounts for this arrangement without agency:
 *   no coalition closed the menu of alternatives; the alternatives lost
 *   late-nineteenth- and early-twentieth-century contests, their
 *   practitioners dispersed, and each succeeding generation transmitted a
 *   thinner record until the surviving form hardened into the assumed shape
 *   of economic life. On this reading the arrangement extracts little
 *   (opportunity costs, diffusely borne, accruing to no pocket), coerces
 *   nothing, and is reversible by an epistemic act — historical recovery —
 *   rather than by dismantling a defended structure. The epsilon referent is
 *   this standing arrangement as this reading assesses it; the sibling
 *   readings assess the same arrangement under different causal accounts and
 *   yield different constraints, filed separately and linked through the
 *   network section. KEY AGENTS (by structural relationship): -
 *   economics_profession: de facto keeper of the transmitted canon
 *   (institutional/constrained) — could widen the menu at modest cost;
 *   collects incidental interpretive authority without coordinating to defend
 *   it - policy_elites: principal inheritors of the default
 *   (powerful/constrained) — bear foreclosed-option costs while enjoying the
 *   deliberation economy - general_public: diffuse bearer of the narrowed
 *   imaginary (powerless/constrained) - alternative_institution_researchers:
 *   recovery-capable outsiders (moderate/mobile) — hold the record, hold no
 *   seat - cooperative_and_commons_movements: residual practitioners
 *   (organized/constrained) — pay a standing re-legitimation cost each
 *   generation
 *
 * KEY AGENTS:
 *   - economics_profession: de facto keeper of the transmitted canon (institutional/constrained) — could restore the alternative menu at modest cost; collects incidental interpretive authority without coordinating to defend it
 *   - policy_elites: principal inheritors of the default (powerful/constrained) — bear foreclosed-option costs while enjoying the deliberation economy
 *   - general_public: diffuse bearer of the narrowed imaginary (powerless/constrained)
 *   - alternative_institution_researchers: recovery-capable outsiders (moderate/mobile) — hold the recoverable record but no seat where the default reproduces itself
 *   - cooperative_and_commons_movements: residual practitioners (organized/constrained) — pay a standing re-legitimation cost each generation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.14).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.12).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.14).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, rope).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market Default via Lapsed Alternatives (Lapsed-Memory Reading)").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political economy/ideology studies/economic history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, '1afe6f03-a75c-458c-bc69-b5723f217432').
narrative_ontology:cs_kernel_codification('1afe6f03-a75c-458c-bc69-b5723f217432', distributed).
narrative_ontology:cs_authority_grounding('1afe6f03-a75c-458c-bc69-b5723f217432', diffuse_epistemic).
narrative_ontology:cs_reading_relation('1afe6f03-a75c-458c-bc69-b5723f217432', market_as_natural_default__beneficiary_maintained_reading, forecloses).
narrative_ontology:cs_reading_relation('1afe6f03-a75c-458c-bc69-b5723f217432', market_as_natural_default__hybrid_amnesia_reading, forecloses).
narrative_ontology:cs_axiom('1afe6f03-a75c-458c-bc69-b5723f217432', foundational, naturalization_requires_no_defender).
narrative_ontology:cs_axiom_status(naturalization_requires_no_defender, holdable).
narrative_ontology:cs_axiom_grounding('1afe6f03-a75c-458c-bc69-b5723f217432', naturalization_requires_no_defender, empirically_contingent).
narrative_ontology:cs_axiom('1afe6f03-a75c-458c-bc69-b5723f217432', foundational, forgotten_alternatives_remain_recoverable).
narrative_ontology:cs_axiom_status(forgotten_alternatives_remain_recoverable, holdable).
narrative_ontology:cs_axiom_grounding('1afe6f03-a75c-458c-bc69-b5723f217432', forgotten_alternatives_remain_recoverable, empirically_contingent).
narrative_ontology:cs_reference_frame('1afe6f03-a75c-458c-bc69-b5723f217432', plural_economic_imagination).
narrative_ontology:cs_drift_state('1afe6f03-a75c-458c-bc69-b5723f217432', contemporary_policy_discourse, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('1afe6f03-a75c-458c-bc69-b5723f217432', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_as_natural_default__lapsed_alternative_reading, economics_profession).
narrative_ontology:constraint_beneficiary(market_as_natural_default__lapsed_alternative_reading, policy_elites).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, policy_elites).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, general_public).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, cooperative_and_commons_movements).
narrative_ontology:constraint_vindicates(market_as_natural_default__lapsed_alternative_reading, institutional_memory_decay_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teaches, writes, and certifies the economic frameworks each cohort inherits. Introductory curricula present market models as the analytically central case; the history of cooperatives, commons governance, and municipal ownership appears, when at all, as historical curiosity rather than live option. No committee decides this and no directive enforces it — the narrowing reproduces through ordinary choices about what fits a semester. The profession could widen the transmitted menu at modest cost, since the scholarship already exists, but faces no pressure to do so, and it collects interpretive authority from the framework's centrality without any coordinated effort to defend that centrality.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, economics_profession, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__lapsed_alternative_reading, economics_profession, beneficiary).

% Staff ministries, legislatures, and development agencies. Reach for market-framing instruments — privatization, vouchers, auctions — as the unmarked first move because trained staff, templates, and precedent all assume them; non-market designs require assembling justification and expertise the default does not stock. They bear the cost of options that never reach their desks, while saving the deliberation cost of reopening first principles — a trade few of them register as a trade.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, policy_elites, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__lapsed_alternative_reading, policy_elites, beneficiary).

% Inherits the default through schooling and media without encountering the record of tested alternatives. Cannot demand arrangements it cannot picture; proposals that do surface — community land trusts, participatory budgeting — arrive stripped of their institutional genealogy and read as novelties rather than recoveries. No barrier blocks curiosity about this history; the obstacle is that nothing in ordinary circulation prompts the question.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, general_public, payer,
    powerless, generational, constrained, global).

% Economic historians, commons scholars, and cooperative-economy researchers hold the recoverable record: archives of functioning mutual aid, worker ownership, and commons governance. They publish in specialty venues the default's carriers do not read and are consulted on allocation questions mainly after market designs fail. Their objection — that the menu was once wide and worked — is available to anyone at library cost, but they hold no seat where the default reproduces itself.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, alternative_institution_researchers, excluded,
    moderate, generational, mobile, global).

% Live practitioners of the residual alternatives — credit unions, worker cooperatives, irrigation associations, community land trusts. Their institutions function but carry a standing tax of explanation: funders, regulators, and recruits treat them as exceptions requiring justification that market forms never owe. Each generation must re-legitimate the model from scratch because the transmitted culture no longer remembers it as ordinary.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, cooperative_and_commons_movements, payer,
    organized, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_as_natural_default__lapsed_alternative_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a shared economic starting frame: a polity cannot re-litigate market-versus-commons-versus-state allocation for every decision in every generation, and an inherited default — markets handle allocation unless specified otherwise — lets law, training, and administration proceed without reopening first principles. The function is real whether or not the specific default is wise.
% TRANSFER_FUNCTION: Moves legitimacy and attention rather than money: market-framed options receive unexamined default standing, while non-market proposals begin from a justification deficit and must assemble evidence the default never owes. No identifiable recipient collects the shift; it dissipates into the background authority of the default itself.
% ABSENT_VOICES: Economic historians, commons scholars, and cooperative theorists would object that the menu of tested arrangements was far wider than the policy conversation admits, and that several entries worked. They sit outside the venues where the default reproduces itself — ministries, business schools, editorial pages — publishing in specialty journals its carriers do not read; they are consulted, if at all, after market designs fail.
% DISAPPEARANCE_RATIONALE: If the default-status vanished overnight — with markets continuing as one option among remembered others — every significant allocation question would reopen its constitutional dimension: procurement, housing, energy, care, and money itself would each require explicit argument for market form against cooperative, commons, or public form. Deliberation costs would spike immediately; over time many arrangements would migrate toward whichever form survived scrutiny, and the policy professions would need rebuilt toolkits. The default is load-bearing as a shared starting frame even on this reading's low-extraction account, so the world rearranges.
% FOUNDING_PROBLEM: The arrangement was not founded to solve anything — under this reading it accreted. As generations passed, the alternatives that lost nineteenth-century contests dropped out of transmitted memory, and the surviving form hardened into the unexamined default. The nearest thing to a founding problem is the recurring need for a shared economic starting frame, which the default came to supply by default rather than by decision.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside any benefiting party: the economic-history literature on cooperatives, commons governance, and municipal ownership attests both that the alternatives existed and functioned at scale and that they dropped from mainstream curricula and policy repertoires; curriculum-content studies and textbook analyses document the narrowing independently. Corroboration from inside the default's routine carriers is absent — finance ministries, business schools, and editorial pages do not register the loss — and under this reading that silence is not a gap in the evidence but the phenomenon itself.
narrative_ontology:disappearance_verdict(market_as_natural_default__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__lapsed_alternative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__lapsed_alternative_reading, 0.14, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_alternative_reading_tests).
:- end_tests(market_as_natural_default__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metric rationale. Extractiveness is authored at 0.14: the arrangement's costs are opportunity costs — options never reaching desks, models never imagined — and under this reading they accrue to no recipient, which caps how extractive a transfer-less arrangement can be. Suppression is 0.12: nothing bars anyone from reading the historical record; the residual figure registers the mild social discount applied to non-market proposals, which this reading attributes to absence of exemplars rather than to enforcement. Theater is 0.08: the canon is transmitted sincerely; there is no performance of defending the default because no one takes themselves to be defending it. Accessibility_collapse is 0.30: once the arrangement is understood as an artifact of lapse, alternatives do not vanish — they are recoverable at library cost, which is why collapse sits far below the natural-law band. Resistance is 0.25: economic historians, commons scholars, and cooperative movements push against the narrowed menu continuously, but from outside the venues where the default reproduces itself.
 *   
 *   Claim/metric independence: claimed_type is rope on structural grounds — the arrangement supplies a genuine coordination function (a shared economic starting frame that spares each generation a full constitutional re-litigation), with minimal coercive overhead, no suppression of alternatives, and no identifiable class collecting from its operation. The metrics are authored separately as the descriptive record of a low-intensity operation. Beneficiaries and victims are intentionally undeclared: the reading's defining structural claim is the absence of an identifiable beneficiary class, and declaring one would fabricate the very structure the reading denies; the directionality_overrides carry the resulting diffuse-incidence structure instead. Measurements run on one shared six-point grid (1870-2020) with both tracked metrics authored at every point; suppression_requirement is deliberately not serialized because the enforcement picture is static — there is no enforcement machinery whose build-up or decay the story tracks — so the scalar suppression stands for the whole interval.
 *
 * PERSPECTIVAL GAP:
 *   The seats diverge sharply for a constraint this mild. From the economics_profession seat the arrangement is nearly invisible — background infrastructure of teaching, experienced as neutrality, with extraction near zero and a faint subsidy of interpretive authority. From the policy_elites seat it reads as efficiency: the default saves deliberation they would otherwise spend, and its costs (options never presented) are invisible precisely because they take the form of absences. From the general_public and movement seats it reads as a wall made of nothing — no adversary, no rule, only the persistent sense that other ways of organizing production are unrealistic. The alternative_institution_researchers seat is structurally distinct from the policy_elites seat despite comparable education and standing: elites sit inside the default's carriage system (templates, trained staff, precedent) and cannot step out of it without cost, while researchers sit outside it and move freely — same nominal level, opposite exit positions. The engine computes these per-seat classifications from the power, horizon, exit, and scope data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim declarations are authored, so the structural derivation has nothing to key on and would fall through to canonical per-power-atom fallbacks — which typically seat low-power agents near the full-target end. That would misstate this reading: the arrangement's burdens are diffuse opportunity costs, not targeted extraction, and its benefits are a deliberation economy enjoyed roughly equally by all inheritors. The overrides therefore place every atom near symmetric: powerless 0.55 and organized 0.55 (the public and the movements carry the imagination and re-legitimation costs, a mild target-side tilt), moderate 0.50 (researchers are exposed mainly as citizens), powerful 0.45 (elites enjoy the deliberation economy slightly more than they pay in foreclosed options), institutional 0.40 (the profession collects incidental interpretive authority — a beneficiary-side tilt that stops well short of capture, since nothing in the record shows that authority being defended). gain_flow is authored as diffuse affirmatively: each named seat was checked for receipt of the arrangement's gains, and none receives them — the costs are opportunity costs that accrue to no pocket, and the profession's incidental authority is a byproduct of the framework's centrality, not a transfer of extracted value.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy risk runs in both directions here. Read as a mountain, the arrangement licenses fatalism — markets are simply how economies are — and forecloses the curricular remedy this reading recommends; read as a snare, it demands culprits the historical record does not clearly contain and converts a memory problem into a prosecution problem. The rope claim keeps the mandate question open: the founding function (a shared starting frame) is arguably still live, so no zombie declaration is issued, and founding_problem_status is authored contested rather than dead. The guard against the flattering-origin-myth failure is the beneficiary_absence_ambiguity omega: if investigation shows incumbents funding the naturalization, this reading collapses into its siblings, the mandate story changes, and the mismatch consumer's dead-mandate-plus-world-rearranges flag becomes the relevant instrument. Until then, the arrangement is best handled as a neglected default whose fix is cheap and whose neglect is the entire pathology.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_absence_ambiguity,
    'Is the absence of an identifiable beneficiary class real, or would a funding and institutional history of the naturalization reveal incumbents who actively maintain it?',
    'Archival and professional-history research: funding trails behind economics curricula, business-school canon formation, and think-tank dissemination of market-framing; correspondence and biographies of canon-setting economists.',
    'Discovering sustained defense collapses this reading into the beneficiary_maintained or hybrid readings: extractiveness rises, a victim structure appears, and classification migrates toward enforced hybrid or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_absence_ambiguity, empirical, 'Whether the no-defender structure is genuine or an unexamined gap in the record.').

omega_variable(
    kernel_reading_indexicality,
    'This story is one reading of the kernel market_as_natural_default; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative evaluation across the three filed readings: whichever causal account the historical evidence sustains identifies which constraint is operative; the disagreement is located in the cause of naturalization (active defense vs. lapsed memory vs. lapse-enabling-capture).',
    'Classification is reading-indexed: epsilon, beneficiary structure, and type differ across siblings over the same standing arrangement; comparing them without the indexical note produces false contradictions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one kernel, three readings, disagreement located in cause.').

omega_variable(
    recovery_completeness,
    'How completely can historical research actually recover the forgotten alternatives, and are any lost beyond practical recovery?',
    'Archive surveys and replication attempts: reconstruct the operational knowledge of historical cooperatives, commons regimes, and municipal ownership; test which recovered designs remain viable under current conditions.',
    'If recovery is substantially incomplete, accessibility_collapse is higher than authored, the rope claim weakens toward inertial persistence, and the curricular remedy loses its sufficiency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recovery_completeness, empirical, 'Whether the epistemic fix this reading recommends is actually available at the claimed cost.').

omega_variable(
    cs_framing_underdetermination,
    'Is the distributed/diffuse_epistemic framing of the kernel''s commitment structure the only defensible one, or does the introductory-economics canon constitute a formalized kernel under expertise authority?',
    'Examine whether any body functions as designated interpreter of the default (standardized curricula, accrediting bodies, canonical textbook cycles) versus purely ambient transmission.',
    'Under the canon-as-kernel framing, authority_grounding shifts to expertise, an interpretation layer becomes valid, and the commitment-system pattern classification changes; the substantive constraint classification is unaffected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Framing under-determination in the commitment-system layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 1870, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lapsed_alternative_reading_tr_t1870, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1870, 0.04).
narrative_ontology:measurement(lapsed_alternative_reading_tr_t1900, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1900, 0.04).
narrative_ontology:measurement(lapsed_alternative_reading_tr_t1930, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1930, 0.05).
narrative_ontology:measurement(lapsed_alternative_reading_tr_t1960, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1960, 0.06).
narrative_ontology:measurement(lapsed_alternative_reading_tr_t1990, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1990, 0.07).
narrative_ontology:measurement(lapsed_alternative_reading_tr_t2020, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 2020, 0.08).

% Extraction over time
narrative_ontology:measurement(lapsed_alternative_reading_be_t1870, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1870, 0.05).
narrative_ontology:measurement(lapsed_alternative_reading_be_t1900, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1900, 0.07).
narrative_ontology:measurement(lapsed_alternative_reading_be_t1930, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1930, 0.09).
narrative_ontology:measurement(lapsed_alternative_reading_be_t1960, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1960, 0.11).
narrative_ontology:measurement(lapsed_alternative_reading_be_t1990, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1990, 0.13).
narrative_ontology:measurement(lapsed_alternative_reading_be_t2020, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 2020, 0.14).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(market_as_natural_default__lapsed_alternative_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__lapsed_alternative_reading, information_standard).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% One colloquial label — market dominance is natural — decomposes, per the epsilon-invariance principle, into three structurally distinct claims that differ in epsilon and beneficiary structure: this file (lapsed_alternative_reading, epsilon approximately 0.14, no beneficiary class), market_as_natural_default__beneficiary_maintained_reading (active post-hoc defense by incumbents; substantially extractive), and market_as_natural_default__hybrid_amnesia_reading (lapse enabling capture; intermediate). The lapsed reading is the low-epsilon baseline of the family; the siblings layer agency onto it. Each file links the other two through affects_constraints; no file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_as_natural_default__lapsed_alternative_reading, powerless, 0.55).
constraint_indexing:directionality_override(market_as_natural_default__lapsed_alternative_reading, organized, 0.55).
constraint_indexing:directionality_override(market_as_natural_default__lapsed_alternative_reading, moderate, 0.5).
constraint_indexing:directionality_override(market_as_natural_default__lapsed_alternative_reading, powerful, 0.45).
constraint_indexing:directionality_override(market_as_natural_default__lapsed_alternative_reading, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
