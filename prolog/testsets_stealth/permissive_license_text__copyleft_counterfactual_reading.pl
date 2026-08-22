% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__copyleft_counterfactual_reading, []).

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
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Permissive License Non-Reciprocity (Copyleft Counterfactual Reading)
 *   domain: economic/technological/legal
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kernel
 *   permissive_license_text: the copyleft counterfactual reading, which holds
 *   that a copyright grant lacking a reciprocity requirement — deployed
 *   beyond the peer-exchange context that produced it — structurally enables
 *   uncompensated enclosure of commons labor, and that viral reciprocity
 *   (GPL-family share-alike) is the necessary corrective. Per the fixed
 *   epsilon referent rule, epsilon here is authored for the STANDING
 *   arrangement under contest — the permissive-default licensing regime as it
 *   operates across the software economy — assessed by this reading's own
 *   lights; it is NOT authored for the GPL arrangement this reading endorses.
 *   The sibling readings are separate constraints in separate files:
 *   commons_coordination_reading (the text's silence maximizes freedom by
 *   minimizing friction) and corporate_moat_reading (the silence is a
 *   rightful input channel for proprietary capture). This file links both via
 *   network.affects_constraints; the family decomposition follows the
 *   epsilon-invariance principle, since the colloquial label 'permissive
 *   licensing' covers structurally distinct claims with distinct epsilon
 *   values, victim sets, and failure modes.
 *
 * KEY AGENTS:
 *   - - proprietary_software_vendors: Primary beneficiary (powerful/arbitrage) — collects derivative value from granted commons code
 *   - - commercial_cloud_providers: Secondary beneficiary (institutional/arbitrage) — monetizes hosted permissive stacks with no source obligation
 *   - - commons_contributors: Primary target (powerless/identity_locked) — supplies unpaid labor absorbed into closed products
 *   - - open_source_maintainers: Secondary target (moderate/constrained) — bears maintenance and liability while integrators commercialize
 *   - - corporate_open_source_programs: Dual-positioned actor (powerful/mobile) — releases strategically, consumes heavily
 *   - - copyleft_advocacy_organizations: Contesting observer (organized/analytical) — campaigns for reciprocity from outside the decision points
 *   - - future_commons_contributors: Excluded party (powerless/trapped) — bound by grants made without them
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.72).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.34).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive License Non-Reciprocity (Copyleft Counterfactual Reading)").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "economic/technological/legal").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, '40375dfa-e936-4476-9c98-a029a60ff6e1').
narrative_ontology:cs_kernel_codification('40375dfa-e936-4476-9c98-a029a60ff6e1', fixed_text).
narrative_ontology:cs_authority_grounding('40375dfa-e936-4476-9c98-a029a60ff6e1', distributed).
narrative_ontology:cs_reading_relation('40375dfa-e936-4476-9c98-a029a60ff6e1', permissive_license_text__commons_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('40375dfa-e936-4476-9c98-a029a60ff6e1', permissive_license_text__corporate_moat_reading, influences).
narrative_ontology:cs_axiom('40375dfa-e936-4476-9c98-a029a60ff6e1', foundational, nonreciprocal_grant_structurally_exploitative).
narrative_ontology:cs_axiom_status(nonreciprocal_grant_structurally_exploitative, holdable).
narrative_ontology:cs_axiom_grounding('40375dfa-e936-4476-9c98-a029a60ff6e1', nonreciprocal_grant_structurally_exploitative, empirically_contingent).
narrative_ontology:cs_axiom('40375dfa-e936-4476-9c98-a029a60ff6e1', foundational, viral_reciprocity_necessary_for_commons_survival).
narrative_ontology:cs_axiom_status(viral_reciprocity_necessary_for_commons_survival, holdable).
narrative_ontology:cs_axiom_grounding('40375dfa-e936-4476-9c98-a029a60ff6e1', viral_reciprocity_necessary_for_commons_survival, instrumental).
narrative_ontology:cs_reference_frame('40375dfa-e936-4476-9c98-a029a60ff6e1', peer_reciprocal_sharing_context).
narrative_ontology:cs_drift_state('40375dfa-e936-4476-9c98-a029a60ff6e1', commercial_enclosure_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('40375dfa-e936-4476-9c98-a029a60ff6e1', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, commercial_cloud_providers).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, commons_contributors).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, open_source_maintainers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, commons_contributors).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, corporate_open_source_programs).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocacy_organizations).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, corporate_open_source_programs).
narrative_ontology:constraint_vindicates(permissive_license_text__copyleft_counterfactual_reading, reciprocity_principle).
narrative_ontology:constraint_vindicates(permissive_license_text__copyleft_counterfactual_reading, copyleft_necessity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build commercial products that embed permissively licensed components: compilers, databases, cryptographic libraries, networking utilities. The grant text imposes no obligation to publish modifications, so derivative value accrues privately. They enforce copyright vigorously over their own closed code while relying on the open grant for inputs. Leaving the arrangement would mean rewriting or commercially licensing equivalent components; instead they arbitrage between suppliers and fund upstream projects only where breakdown threatens a product line.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_vendors, beneficiary,
    powerful, generational, arbitrage, global).

% Operate permissively licensed infrastructure software as metered managed services at planetary scale. Hosted delivery triggers no distribution event, so no source-sharing obligation attaches, and the margin between donated upstream maintenance and billed service accrues to the operator. When an upstream project rewrites its license to close this channel, providers fork the last permissive version or migrate to substitutes.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, commercial_cloud_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Individual developers who submit code, documentation, and fixes to permissively licensed projects, typically unpaid, alongside jobs or studies. They receive working code from strangers in return, which is the arrangement's stated symmetry. When an employer folds their patch into a closed product, no notice or compensation reaches them. Withholding future labor or relicensing is available in principle; in practice most continue because the work and the community are bound up with how they see themselves.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, commons_contributors, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, commons_contributors, beneficiary).

% Volunteer or lightly funded maintainers of widely consumed permissive packages. They triage issues, review patches — frequently from corporate engineers on company time — and carry security exposure for software they give away. They can change the license for future versions, as several have after enclosure episodes, but doing so fractures the user base, invites forks, and draws corporate pressure; recovery of past value is impossible.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, open_source_maintainers, payer,
    moderate, biographical, constrained, global).

% Companies running deliberate open-source strategies: releasing internally developed infrastructure under permissive licenses to set standards and commoditize competitors' complementary layers, while consuming the common stock heavily. They fund foundations, employ maintainers, and shape license-choice norms through procurement and hiring — and they bear real costs when rivals free-ride on their releases, which is why the same firms sometimes lead license-change fights.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, corporate_open_source_programs, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, corporate_open_source_programs, payer).

% Foundations and membership organizations that draft and defend reciprocity-bearing licenses, pursue compliance actions, and campaign against one-way commercialization. They sit outside the arrangement's decision points — no permissive project must answer to them — while drawing members, donations, and relevance from each enclosure controversy. Their remedy, mandatory share-alike, is structurally unavailable inside the standing default.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocacy_organizations, observer,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocacy_organizations, beneficiary).

% People not yet in the room when grant decisions are made: students, career-changers, and future maintainers whose labor pool the accumulated permissive corpus will govern. Each grant made today binds code they will build on tomorrow, and none were consulted; their only recourse is whatever license each new project chooses at publication.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, future_commons_contributors, excluded,
    powerless, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(permissive_license_text__copyleft_counterfactual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, zero-negotiation legal instrument that lets unrelated parties combine, modify, and redistribute code; uniform grant terms create a predictable common pool and an interoperability baseline across organizational boundaries.
% TRANSFER_FUNCTION: Moves contributed development and maintenance labor — and the derivative product value built upon it — from volunteer contributors and maintainers to commercial integrators who enclose improvements in closed or hosted offerings without reciprocal publication.
% ABSENT_VOICES: Future contributors and downstream commons participants had no seat at grant time; non-commercial users without legal counsel are unrepresented in license-policy fights; and the reciprocating counterparties — firms that would contribute back under a share-alike default — are absent precisely because the default selects against them.
% DISAPPEARANCE_RATIONALE: The permissive corpus underpins cloud stacks, developer toolchains, embedded products, and AI training corpora. Overnight removal would force mass relicensing negotiations or abandonment: shipped products containing now-unlicensed code, cloud services losing their software base, and a scramble to reimplement or purchase equivalents — a rearrangement at civilizational-infrastructure scale.
% FOUNDING_PROBLEM: Case-by-case permission friction: early-1980s Berkeley distributors needed a standard grant letting academic peers reuse and redistribute Unix-derived code without negotiating with counsel each time; the permissive text was drafted to make sharing among trusted peers instantaneous.
% FOUNDING_PROBLEM_CORROBORATION: Computing-history scholarship on the CSRG/BSD origin attests the peer-exchange purpose from outside the benefiting parties. Behavior corroborates obsolescence from inside: the 2018–2024 wave of conversions away from permissive terms (MongoDB, Confluent, Elastic, HashiCorp, Redis) is revealed-preference testimony by former beneficiaries that instantaneous peer sharing no longer describes how the corpus is consumed. Current beneficiaries — vendors and cloud operators — deny obsolescence and attest the opposite; no disinterested party attests the founding problem remains live.
narrative_ontology:disappearance_verdict(permissive_license_text__copyleft_counterfactual_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__copyleft_counterfactual_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__copyleft_counterfactual_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are independent authored facts. From this reading's seat the standing arrangement is structurally a tangled_rope: the grant text performs a real coordination function (standardized zero-negotiation reuse) AND the same structure carries asymmetric transfer toward commercial integrators — hence claimed_type tangled_rope with requires_active_enforcement true. Enforcement is real but selective: copyright machinery is wielded vigorously to protect enclosed derivatives and permissive-term compliance, while no comparable machinery enforces any return flow; the asymmetry is what the enforcement requirement records. Extractiveness is high (0.72) because the transfer is decoupled from any reciprocal obligation and scales with the corpus. Suppression is moderate-low (0.34) and UNSCALED by construction: nothing coerces a grant at authoring time — alternatives (copyleft licensing) remain choosable — so the coercive content lives in post-grant lock-in and selective enforcement, not in the grant itself. Theater is low (0.18): the instrument is short, functional, and mostly does what it says; the slow rise tracks attribution compliance becoming ritual and open-washing by non-contributing firms. Accessibility_collapse is moderate (0.42): once the enclosure dynamic is understood, any author may still license copyleft, but network effects, corporate compatibility pressure, and dependency-chain inertia progressively erode that alternative in practice. Resistance is substantial (0.55): license-conversion waves, AGPL adoption, and advocacy campaigns are ongoing active contestation of the default. All three tracked series share one six-point grid (1995–2025); endpoint values equal the base_properties scalars. The extractiveness series rises monotonically — accumulation, not oscillation — driven by cloud delivery (no distribution trigger) and, latterly, model training on permissive corpora.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute sharply different types from identical structural data. From the vendor and cloud seats the arrangement is a functioning gift economy they did not design and do not administer — they simply take what the text grants, and their exit is arbitrage-grade, placing them near the beneficiary pole. From the contributor and maintainer seats the same text operates as a one-way valve: labor out, enclosure back, with exit blocked by identity fusion (contributors) or retrofit impossibility (maintainers). The advocacy seat sees the trap the payer seats inhabit; the corporate open-source-program seat straddles, releasing strategically while absorbing the same flows. The engine computes these per-seat divergences from the structural data; this reading's claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (proprietary_software_vendors, commercial_cloud_providers) drive low derived directionality, amplified toward the beneficiary end by arbitrage-grade exit. Victim declarations (commons_contributors, open_source_maintainers) drive high derived directionality, pushed toward the full-target end by identity_locked and constrained exit respectively. Two overrides correct derivation errors the structural data alone cannot fix: (1) powerless agents are pinned to d=0.80 because commons_contributors carry a secondary beneficiary role (they consume others' code) that would damp the derived value below the reading-assessed net position — the labor-out/enclosure-back asymmetry dominates their incidental consumption — and future_commons_contributors share the same commons-side exposure; (2) organized agents are pinned to d=0.52 because copyleft_advocacy_organizations hold a secondary beneficiary role (donations and relevance flow from each enclosure controversy) that would otherwise derive them near the beneficiary pole, when their actual position is a contesting observer with incidental sustenance. Corporate_open_source_programs receive no override: their dual beneficiary/payer roles are left to the engine to net, which is the honest treatment of a genuinely ambivalent seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two symmetrical mislabels. Reading the arrangement as a pure rope (the commons reading's implicit verdict) would erase the victim structure — treating uncompensated enclosure as coordination cost. Reading it as a snare would erase the genuine grant function and the voluntariness at authoring time; nobody is forced into the arrangement, which is disqualifying for a snare and exactly what a tangled_rope accommodates. On genealogy: the founding problem (peer-exchange permission friction) is dead while the arrangement not merely persists but governs more value than ever — the founding_problem_status x disappearance_verdict mismatch (dead + world_rearranges) correctly routes this story to the capture/zombie investigation path, which is precisely this reading's drift thesis formalized. Theater stays low, so the piton path should not fire: the instrument still functions; what changed is who it serves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the copyleft_counterfactual_reading of kernel permissive_license_text; would the sibling readings (commons_coordination_reading, corporate_moat_reading) classify the same standing arrangement differently, and on which structural element?',
    'Generate the sibling stories against the identical structural referent (the standing permissive-default arrangement) and compare computed types; divergence localizes the disputed element.',
    'If commons_coordination_reading computes a low-epsilon rope with no victims of record, the dispute reduces to whether uncompensated enclosure counts as harm; if corporate_moat_reading computes high extraction without any harmed seat, the dispute is purely normative entitlement. Either result changes this reading''s beneficiary/victim structure and therefore its per-seat classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of the permissive-license kernel; siblings instantiate different constraints over the same text.').

omega_variable(
    enclosure_value_transfer,
    'How much value annually moves from commons contributors and maintainers to proprietary integrators and cloud operators via non-reciprocal grants?',
    'Economic studies contrasting open-source supply-side contribution value with demand-side commercial value; firm disclosures of revenue attributable to permissively licensed components versus contributions returned upstream.',
    'A small sustained transfer would pull epsilon down toward the coordination-cost range and soften the victim structure; a large, growing transfer supports the high-epsilon reading and warrants monitoring for drift toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enclosure_value_transfer, empirical, 'Magnitude of the uncompensated labor-and-value flow the reading asserts.').

omega_variable(
    reciprocity_necessity,
    'Is viral reciprocity strictly necessary to prevent enclosure, or would lighter mechanisms (source-available terms, contributor agreements, patronage, public funding) suffice?',
    'Comparative trajectories of projects under comparable enclosure pressure that chose copyleft versus alternatives — the Redis, Elastic, and HashiCorp conversions against PostgreSQL-model and foundation-stewardship projects.',
    'If substitutes hold, the necessity axiom weakens and the standing arrangement reads less trap-like; if substitutes repeatedly fail under pressure, the reading''s foundational claim strengthens and the arrangement''s persistence looks more deliberately maintained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_necessity, conceptual, 'Whether the endorsed alternative is necessary or merely sufficient among several.').

omega_variable(
    contributor_identity_lock,
    'Is contributor persistence under uncompensated enclosure maintained by genuine commons commitment or by identity lock that suppresses exit?',
    'Post-exit suppression trajectory: track contributors who disengage or move to proprietary work — if grievance and continued unpaid contribution patterns persist after the immediate incentives change, the lock is partly internalized.',
    'An internalized component raises effective suppression above the structural measure for the payer seat and changes how the engine weights that seat''s trapped-or-locked status; a purely structural reading keeps suppression at the authored scalar.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contributor_identity_lock, empirical, 'Structural versus internalized mechanism behind contributor persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t1995, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(perm_tr_t2001, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 2001, 0.11).
narrative_ontology:measurement(perm_tr_t2007, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 2007, 0.13).
narrative_ontology:measurement(perm_tr_t2013, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 2013, 0.15).
narrative_ontology:measurement(perm_tr_t2019, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 2019, 0.17).
narrative_ontology:measurement(perm_tr_t2025, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(perm_be_t1995, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(perm_be_t2001, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 2001, 0.46).
narrative_ontology:measurement(perm_be_t2007, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 2007, 0.55).
narrative_ontology:measurement(perm_be_t2013, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 2013, 0.63).
narrative_ontology:measurement(perm_be_t2019, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 2019, 0.69).
narrative_ontology:measurement(perm_be_t2025, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t1995, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 1995, 0.22).
narrative_ontology:measurement(perm_su_t2001, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 2001, 0.24).
narrative_ontology:measurement(perm_su_t2007, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 2007, 0.27).
narrative_ontology:measurement(perm_su_t2013, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 2013, 0.3).
narrative_ontology:measurement(perm_su_t2019, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 2019, 0.32).
narrative_ontology:measurement(perm_su_t2025, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 2025, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, information_standard).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__corporate_moat_reading).

% DUAL FORMULATION NOTE:
% Constraint family for kernel permissive_license_text, decomposed per the epsilon-invariance principle: the colloquial label 'permissive licensing' conflates three structurally distinct claims. commons_coordination_reading authors low epsilon over a friction-reduction claim (rope-shaped); corporate_moat_reading authors high extraction treated as entitlement with no harmed seat of record; this reading authors high epsilon over the same standing arrangement WITH a victim structure and a necessity claim for the alternative. Edges: the commons reading is upstream (its legitimacy narrative is what the other two contest and what makes the default self-maintaining); this reading exerts downstream pressure on the corporate moat reading (share-alike adoption and license conversions raise the moat strategy's costs) without foreclosing it. Each sibling file carries its own epsilon, beneficiaries, victims, and claimed type; this file's dual_formulation_note documents the split for all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(permissive_license_text__copyleft_counterfactual_reading, powerless, 0.8).
constraint_indexing:directionality_override(permissive_license_text__copyleft_counterfactual_reading, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
