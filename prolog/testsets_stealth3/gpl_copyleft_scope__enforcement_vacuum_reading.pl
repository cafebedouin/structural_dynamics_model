% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__enforcement_vacuum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__enforcement_vacuum_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__enforcement_vacuum_reading
 *   human_readable: GPL Copyleft Scope — Enforcement-Vacuum Plurality Reading
 *   domain: legal/software_licensing/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the enforcement_vacuum_reading of the
 *   gpl_copyleft_scope kernel: because no definitive judicial precedent fixes
 *   the GPL Section 2(b) derivative-work boundary for linked or combined
 *   works, the strong-copyleft and narrow-scope readings coexist as a
 *   licensed plurality, and the constraint actually binding any given actor
 *   depends on which interpretive community holds enforcement capacity in
 *   that actor's context. The arrangement solves a real coordination problem
 *   — it lets a plural ecosystem keep building on shared code without a
 *   forced binary ruling — while extracting asymmetrically: clarity-seeking
 *   adopters pay elevated transaction costs for certainty that is never
 *   delivered, unaffiliated upstream authors' intended protections fail
 *   silently in commercially governed contexts, and downstream users lose
 *   promised freedoms without any seat in the contest. The claim and metrics
 *   are independent authored facts: claimed_type records what I believe is
 *   structurally true of this arrangement (a hybrid with genuine coordination
 *   and real extraction); the metric values record what I believe is
 *   descriptively true of its operation. Per the epsilon-invariance principle
 *   this is one member of a three-story constraint family — the sibling
 *   readings are separate files with their own referents, epsilon values, and
 *   beneficiary structures, linked via network.affects_constraints. Time
 *   mapping: t = year - 1991, so t0 approximates the post-GPLv2 baseline
 *   (1992) and t32 the present landscape (2024).
 *
 * KEY AGENTS:
 *   - - fsf_and_copyleft_stewards: Agenda-setting administrator ([institutional]/[identity_locked]) — authors, stewards, and selectively enforces the license; mission-fused participation
 *   - - industry_dominated_ecosystems: Principal beneficiary ([institutional]/[arbitrage]) — holds enforcement capacity in its contexts; sets operative readings where it governs
 *   - - pragmatic_commercial_adopters: Secondary beneficiary ([powerful]/[constrained]) — converts ambiguity into option value under managed risk
 *   - - clarity_seeking_adopters: Payer ([moderate]/[constrained]) — purchases certainty the arrangement does not deliver
 *   - - independent_upstream_developers: Dual-positioned payer/beneficiary ([moderate]/[identity_locked]) — intends protections that industry contexts fail to enforce, gains reach they could not buy
 *   - - licensing_intermediaries: Beneficiary ([moderate]/[mobile]) — monetizes the recurring question
 *   - - downstream_free_software_users: Excluded payer ([powerless]/[trapped]) — bears diluted protections with no seat in the contest
 *   - - ip_litigation_judiciary: Analytical observer ([institutional]/[analytical]) — sees the full structure; thin docket by arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__enforcement_vacuum_reading, 0.45).
domain_priors:suppression_score(gpl_copyleft_scope__enforcement_vacuum_reading, 0.4).
domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__enforcement_vacuum_reading, "GPL Copyleft Scope — Enforcement-Vacuum Plurality Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__enforcement_vacuum_reading, "legal/software_licensing/intellectual_property").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__enforcement_vacuum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, '82cf3313-7972-434c-aae3-fad9d474038e').
narrative_ontology:cs_kernel_codification('82cf3313-7972-434c-aae3-fad9d474038e', fixed_text).
narrative_ontology:cs_authority_grounding('82cf3313-7972-434c-aae3-fad9d474038e', distributed).
narrative_ontology:cs_reading_relation('82cf3313-7972-434c-aae3-fad9d474038e', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('82cf3313-7972-434c-aae3-fad9d474038e', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('82cf3313-7972-434c-aae3-fad9d474038e', foundational, operative_constraint_is_capacity_determined).
narrative_ontology:cs_axiom_status(operative_constraint_is_capacity_determined, holdable).
narrative_ontology:cs_axiom_grounding('82cf3313-7972-434c-aae3-fad9d474038e', operative_constraint_is_capacity_determined, empirically_contingent).
narrative_ontology:cs_axiom('82cf3313-7972-434c-aae3-fad9d474038e', secondary, interpretive_pluralism_is_stable_equilibrium).
narrative_ontology:cs_axiom_status(interpretive_pluralism_is_stable_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('82cf3313-7972-434c-aae3-fad9d474038e', interpretive_pluralism_is_stable_equilibrium, conventional).
narrative_ontology:cs_reference_frame('82cf3313-7972-434c-aae3-fad9d474038e', licensed_interpretive_pluralism).
narrative_ontology:cs_drift_state('82cf3313-7972-434c-aae3-fad9d474038e', contemporary_regulatory_clarity_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('82cf3313-7972-434c-aae3-fad9d474038e', '2026-08-10T00:00:00Z').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_ecosystems).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_commercial_adopters).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, licensing_intermediaries).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, independent_upstream_developers).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, downstream_free_software_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, independent_upstream_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish and steward the license text, run compliance initiatives, and periodically bring infringement actions (embedded-device and firmware cases) to defend the sharing terms. Their enforcement capacity is real but selectively deployable: each action carries reputational and doctrinal risk, since an adverse ruling would settle the boundary against them permanently. Walking away from the license they authored would dissolve the organization's purpose, so their participation is continuous regardless of outcomes.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_and_copyleft_stewards, agenda_setter,
    institutional, generational, identity_locked, global).

% Corporate-governed foundations and platform vendors whose products incorporate GPL components under combination architectures they control. In their contexts they set the prevailing interpretation through internal compliance regimes, contribution policies, and the practical ability to litigate or absorb disputes. They can route new development toward permissively licensed alternatives whenever GPL terms threaten product roadmaps, which gives them leverage over the terms of engagement in every negotiation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_ecosystems, beneficiary,
    institutional, generational, arbitrage, global).

% Product teams that incorporate GPL-licensed components and manage the scope question through risk registers rather than resolved doctrine: they document assumptions, isolate linkage patterns where convenient, and proceed wherever exposure seems manageable. They gain speed and option value from the unsettled boundary but carry ongoing review burdens and tail risk from a contrary future ruling.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_commercial_adopters, beneficiary,
    powerful, biographical, constrained, global).

% Organizations, often in regulated industries, that need determinate answers before shipping: they purchase licensing audits, impose conservative isolation architectures that forgo functionality, or abandon otherwise attractive components entirely. Their spending buys certainty the current arrangement does not actually sell.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters, payer,
    moderate, biographical, constrained, global).

% Individual and small-team authors who license work under the GPL intending that downstream users receive source and freedoms. When their code is absorbed into commercially governed combinations, the operative reading applied is whatever the absorbing context enforces, which frequently is not the one they intended; yet the same absorption brings their work reach and maintenance they could not obtain alone.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, independent_upstream_developers, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, independent_upstream_developers, beneficiary).

% Law firms, consultancies, and compliance-tooling vendors whose practice volume tracks the unsettled boundary: audits, opinion letters, training, and remediation engagements recur precisely because no ruling closes the question. Their revenue depends on the question staying open long enough to bill through, but not so violently open that clients stop integrating altogether.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, licensing_intermediaries, beneficiary,
    moderate, biographical, mobile, global).

% End users, device owners, and downstream distributors who rely on the promise that GPL-covered software ships with source and redistribution rights. When combinations are read narrowly in commercial contexts, the protections they were promised quietly fail to materialize; they hold no seat in the interpretive contest that decides this.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, downstream_free_software_users, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, downstream_free_software_users, excluded).

% Courts and specialized intellectual-property practitioners who adjudicate the occasional dispute. Each case arrives carrying ecosystem-shaping weight, which is precisely why disputants avoid testing the boundary unless cornered; the judiciary's analytical distance is real, but its relevant docket stays thin because the arrangement routes most conflicts away from formal resolution.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, ip_litigation_judiciary, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_ecosystems).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__enforcement_vacuum_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The unresolved boundary coordinates a plural software economy: it lets incompatible interpretive commitments coexist without a forced binary litigation outcome, lets each ecosystem settle local practice, and lets adoption decisions proceed under risk frameworks instead of stalling on a question copyright doctrine cannot currently answer.
% TRANSFER_FUNCTION: Moves interpretive authority and compliance burden: decision rights over what counts as an infringing combination flow toward whoever holds enforcement capacity in a given context; clarity-seeking adopters transfer money and engineering effort to legal intermediaries and conservative architectures; pragmatic adopters and capacity-holding ecosystems capture flexibility and control that upstream authors and downstream users do not receive.
% ABSENT_VOICES: Downstream free-software users and unaffiliated upstream authors would object that 'licensed plurality' means their intended protections fail silently wherever industry capacity sets the reading, but they are absent from the litigation calculus, underrepresented in foundation governance, and unrepresented in standard-setting bodies dominated by corporate members. Smaller jurisdictions' procurement and consumer-protection authorities are similarly outside the room.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — say a definitive ruling landed on Section 2(b)'s scope — thousands of shipped products would face immediate reclassification of their linkages. A strong-reading verdict would force mass relicensing, architectural rework, or withdrawal of GPL components from commercial products; a narrow-reading verdict would void the copyleft protection on most combined works at a stroke. Either way the current division of interpretive labor among camps collapses and is rebuilt around the ruling.
% FOUNDING_PROBLEM: Late-1980s GPL drafting needed a sharing clause that prevented proprietary enclosure of collectively built code, but copyright doctrine offered no determinate answer for program linking; rather than stall adoption on an unanswerable legal question, the ecosystem proceeded with the text left ambiguous and let practice accumulate in the space no ruling occupied.
% FOUNDING_PROBLEM_CORROBORATION: Academic copyright scholarship attests from outside all benefiting parties that derivative-work doctrine genuinely lacks a determinate answer for linking, corroborating the founding indeterminacy; the unresolved litigation record (decades of disputes settled or abandoned short of appellate determination on this precise question) independently attests that the founding problem was never closed. On whether the founding problem remains live, the parties dispute: copyleft stewards cite continuing enclosure attempts including network-service delivery, while industry participants attest the sharing bargain is met through alternative mechanisms — neither attestation comes from a disinterested seat, which is itself recorded here as signal.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__enforcement_vacuum_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__enforcement_vacuum_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_copyleft_scope__enforcement_vacuum_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).
:- end_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.45 at interval end) rather than high because the arrangement's referent, assessed by this reading's own lights, includes genuine coordination value — the ambiguity buffers a plural ecosystem against a catastrophic binary ruling and keeps adoption flowing. Extraction is nonetheless well above a pure-coordination floor because the costs it generates are asymmetrically distributed along enforcement-capacity lines: clarity-seekers buy certainty that is never sold, upstream intent is silently overridden where capacity sits elsewhere, and decision rights concentrate in capacity-holding contexts. Suppression (0.40) reflects structural deterrence — litigation-tail risk and capacity asymmetry discourage challenges — rather than coercive force; alternatives (permissive licenses, dual licensing, avoidance) remain available, hence low accessibility_collapse (0.35). Theater (0.35) captures defensive compliance formalism: notice files, policy documents, and audit artifacts maintained partly for appearance while actual coupling practice follows local convenience and capacity. Resistance (0.55) is real: avoidance strategies, license-version politics, and episodic litigation threats contest the arrangement continuously. All three tracked series run on one shared nine-point grid (every 4 units across 0–32) so no metric borrows another's end-state values; final values match the base_properties scalars. The series show two full enforcement-episode cycles (suppression spikes with corresponding extraction dips near t16 and t28, matching the major enforcement campaigns of the late 2000s and circa 2020): the oscillation is itself part of the mechanism — episodic enforcement chills opportunistic coupling temporarily (intermittent reinforcement), after which relaxation lets extraction re-accumulate; base_properties were measured at the post-relaxation accumulation phase (t32). No directionality_overrides are declared: the schema keys overrides per power atom, and this story's shared atoms (two institutional seats, three moderate seats) span sharply different structural relationships, so any override would collide across agents the derivation already distinguishes correctly through beneficiary/victim declarations and exit options. Coordination type is identity_coordination because the dominant function is boundary maintenance between interpretive camps (copyleft-committed versus industry-pragmatic): what fails first if the arrangement broke is the coexistence of camp identities, not any resource allocation. The conservative floor for this type is appropriate here — the identity dimension is genuine, but identity framing also carries the extraction riding on it.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the stewards' seat the arrangement is a defended frontier whose every concession erodes mission; from the industry-ecosystem seat it is a workable pluralism that merely requires local capacity; from the clarity-seeker seat it is expensive fog; from the upstream author's seat it is a promise honored in some contexts and voided in others; from the intermediary's seat it is recurring billable complexity. Two same-level divergences deserve note. First, clarity_seeking_adopters and licensing_intermediaries hold the identical moderate power atom yet occupy opposite directionalities — differentiated entirely by declared role and exit options (constrained payer versus mobile collector), which is exactly the lateral-differentiation surface the engine reads. Second, the stewards and industry ecosystems hold the identical institutional power atom with divergent enforcement capacity, and that capacity difference — not nominal power — is this reading's core structural claim. Coalition potential exists among the weaker payers (upstream authors and clarity-seekers could coordinate amicus positions or shared audit standards), but coordination costs across heterogeneous interests have historically kept such coalitions episodic.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (industry_dominated_ecosystems, pragmatic_commercial_adopters, licensing_intermediaries) derive low directionality — the arrangement subsidizes them: ecosystems gain operative control in their contexts with arbitrage-grade exit that lets them exploit the ambiguity itself; pragmatic adopters convert uncertainty into option value; intermediaries monetize recurrence. Declared victims (clarity_seeking_adopters, independent_upstream_developers, downstream_free_software_users) derive high directionality — clarity-seekers bear the transaction-cost transfer with constrained exit; upstream authors are identity-locked participants whose intended constraint fails precisely where enforcement capacity lies elsewhere, pushing them toward the full-target end despite their incidental adoption gains; downstream users are trapped and powerless, bearing diffuse losses with no exit at all. The stewards appear in neither array: they administer the arrangement while its operative effect partially defeats their intent, so their directionality falls to the canonical fallback rather than structural derivation — a residual limitation recorded here rather than papered over with a power-atom override that would also hit the industry-ecosystem seat. Suppression is authored as a raw structural property and is intentionally NOT scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim keeps both halves visible. Reading the arrangement as pure coordination would hide who pays: the buffer function is real, but the buffer's costs land on seats that never agreed to fund it, and the allocation of interpretive authority follows capacity rather than merit or consent. Reading it as pure extraction would mispredict collapse: the coordination function genuinely holds the ecosystem together, and the arrangement has survived every prediction that clarity demands would destroy it, because each camp's local practice delivers enough value to keep participation rational. On obsolescence: the founding problem (preventing enclosure of shared code) is contested rather than dead — enclosure attempts continue in new forms (network services, restrictive device firmware) while industry participants attest alternative mechanisms suffice — and the ambiguity-buffer function is unambiguously live, so no zombie mismatch (dead-status-plus-world-rearranges) is authored here. The constraint's persistence is maintained by live function plus capacity asymmetry, not by inertia or performance alone; theater_ratio at 0.35 reflects defensive formalism layered on a functioning, if asymmetric, structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precedent_resolution_outcome,
    'Will definitive judicial precedent on the GPL Section 2(b) scope emerge, and if so, which reading does it vindicate?',
    'Track high-profile infringement litigation through appellate judgment on linking or combination architecture, plus regulatory codifications (security-provenance and licensing-disclosure mandates) that force determinate license interpretations into procurement.',
    'A strong-reading vindication converts this arrangement toward broad-target extraction — integrators at scale become victims and the stewards'' beneficiary position sharpens dramatically; a narrow-reading vindication dissolves the ambiguity rents, intermediaries'' practice base contracts, and this story''s operative constraint decays toward vestigial coordination; continued absence sustains exactly the plurality this reading models.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_resolution_outcome, empirical, 'Whether and how the interpretive plurality resolves, and with what redistributive consequence.').

omega_variable(
    kernel_reading_instantiation,
    'This constraint is the enforcement_vacuum_reading of the gpl_copyleft_scope kernel — what structurally changes if a sibling reading is instantiated instead?',
    'Conceptual comparison across the family: this story''s referent is the capacity-determined operative constraint; the strong_copyleft_reading story''s referent is the maximal semantic obligation; the narrow_scope_reading story''s referent is the traditional-doctrine boundary. The disagreement lives in which element is the operative constraint, not in any measurement parameter inside one story.',
    'Instantiating strong_copyleft_reading yields a higher-extractiveness constraint with a far broader victim set (every commercial integrator of coupled works) and concentrated copyleft-steward beneficiaries; instantiating narrow_scope_reading yields a low-extractiveness constraint with industry ecosystems near the full-beneficiary pole and upstream-author intent unprotected by construction. This story''s metrics are valid only for its own referent and are invalid inputs for either sibling''s classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one of three readings of one kernel, with sibling stories carrying the other referents.').

omega_variable(
    enforcement_capacity_measures,
    'What observable operations constitute enforcement capacity for an interpretive community, and can the capacity asymmetry this reading posits be measured rather than assumed?',
    'Audit enforcement actions attributable to each camp (demand-letter volumes, funded suits, settlement terms), compliance-infrastructure investment (program headcount, tooling), foundation governance rules that pre-commit members to a reading, and dispute outcomes correlated with camp affiliation.',
    'If capacity proves symmetric across camps, or if informal reputation sanctions dominate formal enforcement everywhere, the capacity-determination premise weakens and the constraint''s cost profile shifts from directed allocation of interpretive authority toward diffuse transaction costs — moving the classification''s center of gravity from tangled_rope toward rope-with-overhead. If capacity asymmetry measures larger than assumed, extraction concentration increases and the arrangement trends toward the snare boundary in industry-governed contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_measures, empirical, 'Measurability and magnitude of the capacity asymmetry that this reading takes as the operative variable.').

omega_variable(
    suppression_mechanism_split,
    'Is the measured suppression structural (litigation-tail risk, capacity asymmetry, exclusion from interpretive fora) or internalized (organizational risk cultures that outlast the enforcement episodes that formed them)?',
    'Post-episode behavioral trajectory analysis: if conservative integration choices and avoidance policies persist undiminished after enforcement activity recedes and no new threats emerge, the internalized component is confirmed; if avoidance scales back with the threat environment, suppression is predominantly structural.',
    'An internalized component raises effective suppression above the structural measure — organizations carry the deterrent with them after the deterrent''s cause passes, entrenching the clarity-seekers'' overpayment and the upstream authors'' unenforced intent; a purely structural profile means removal of enforcement-capacity asymmetry would rapidly restore contestability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized composition of the arrangement''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(gpl__tr_t4, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement(gpl__tr_t8, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(gpl__tr_t12, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(gpl__tr_t16, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(gpl__tr_t24, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(gpl__tr_t28, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 28, 0.33).
narrative_ontology:measurement(gpl__tr_t32, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 32, 0.35).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gpl__be_t4, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 4, 0.3).
narrative_ontology:measurement(gpl__be_t8, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(gpl__be_t12, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(gpl__be_t16, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(gpl__be_t24, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(gpl__be_t28, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 28, 0.36).
narrative_ontology:measurement(gpl__be_t32, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 32, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(gpl__su_t4, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 4, 0.23).
narrative_ontology:measurement(gpl__su_t8, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 8, 0.27).
narrative_ontology:measurement(gpl__su_t12, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 12, 0.31).
narrative_ontology:measurement(gpl__su_t16, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(gpl__su_t20, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(gpl__su_t24, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 24, 0.36).
narrative_ontology:measurement(gpl__su_t28, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 28, 0.46).
narrative_ontology:measurement(gpl__su_t32, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 32, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__enforcement_vacuum_reading, identity_coordination).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__narrow_scope_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the GPL's copyleft scope' covers three structurally distinct constraints, written as three stories. The strong_copyleft_reading story models the maximal semantic obligation (high epsilon, broad victim set); the narrow_scope_reading story models the traditional-doctrine boundary (low epsilon, industry ecosystems near the beneficiary pole); this story models the enforcement-vacuum plurality itself — the meta-arrangement in which the other two coexist unresolved and capacity decides. The upstream sibling (strong_copyleft_reading) influences this one: its asserted obligation is precisely what the vacuum leaves unenforced, and advocacy for it shapes enforcement-capacity investments. All three stories link one another via network.affects_constraints; each carries its own epsilon, beneficiaries, and victims, and no story hedges across referents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
