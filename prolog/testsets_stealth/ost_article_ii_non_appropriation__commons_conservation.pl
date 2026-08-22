% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__commons_conservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__commons_conservation, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__commons_conservation
 *   human_readable: OST Article II Non-Appropriation Wall — Commons Conservation Reading
 *   domain: legal/international-commons
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel: Outer Space
 *   Treaty Article II. The commons_conservation reading holds that
 *   'appropriation' in the text's ordinary meaning reaches de facto
 *   appropriation accomplished through resource extraction, and that the
 *   prohibition binds states and their nationals alike — a wall: no
 *   extraction absent multilateral authorization. The standing arrangement
 *   under contest, and therefore the epsilon referent, is the Article II
 *   non-appropriation regime as applied to resource extraction, assessed by
 *   this reading's own lights: the wall is legitimate governance, but its
 *   costs land asymmetrically, and its enforcement machinery has thinned as
 *   national statutes and a parallel accords framework openly practice the
 *   rival reading. KEY AGENTS (by structural relationship):
 *   non_spacefaring_states — primary beneficiary (organized/constrained),
 *   collects veto and option value without development costs;
 *   major_spacefaring_states — dual payer/beneficiary (institutional/mobile),
 *   bears restraint, collects stability, holds agenda power;
 *   asteroid_mining_companies and lunar_resource_operators — primary targets
 *   (moderate/arbitrage), first-mover capital exposed to stranding, exiting
 *   by jurisdiction shopping; scientific_open_access_community — secondary
 *   beneficiary (moderate/constrained); copuos_legal_subcommittee — agenda
 *   setter (institutional/constrained), convenes interpretation without
 *   enforcement power; artemis_accords_signatories — defecting payers
 *   (institutional/arbitrage) on a parallel track;
 *   space_law_scholarly_community — analytical observer. Claim and metrics
 *   are authored independently: the type claim is tangled_rope from
 *   structure; the metric values describe observed operation. This file is
 *   one member of a three-story family decomposing the Article II label; the
 *   siblings are separate constraints with their own epsilon, victims, and
 *   classification.
 *
 * KEY AGENTS:
 *   - non_spacefaring_states: Primary beneficiary (organized/constrained) — collects veto over enclosure and the option value of the open commons without bearing development costs
 *   - major_spacefaring_states: Dual-positioned payer/beneficiary (institutional/mobile) — bears restraint costs, collects anti-scramble stability, holds agenda power, holds the best exits
 *   - asteroid_mining_companies: Primary target (moderate/arbitrage) — first-mover capital exposed to stranding; exits by jurisdiction shopping
 *   - lunar_resource_operators: Primary target (moderate/arbitrage) — polar site tenure insecure under the wall
 *   - scientific_open_access_community: Secondary beneficiary (moderate/constrained) — open-access guarantee protects research sites at no restraint cost
 *   - copuos_legal_subcommittee: Agenda setter (institutional/constrained) — convenes the interpretive forum, holds no enforcement power
 *   - artemis_accords_signatories: Defecting payers (institutional/arbitrage) — exited into a parallel authorization framework rather than amending or leaving the treaty
 *   - space_law_scholarly_community: Analytical observer (analytical/analytical) — maps the reading contest, supplies external corroboration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, 0.5).
domain_priors:suppression_score(ost_article_ii_non_appropriation__commons_conservation, 0.58).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__commons_conservation, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, extractiveness, 0.5).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__commons_conservation, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__commons_conservation, "OST Article II Non-Appropriation Wall — Commons Conservation Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__commons_conservation, "legal/international-commons").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__commons_conservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__commons_conservation, 'd8961e5e-da6a-4654-ba4a-513fd50fc830').
narrative_ontology:cs_kernel_codification('d8961e5e-da6a-4654-ba4a-513fd50fc830', fixed_text).
narrative_ontology:cs_authority_grounding('d8961e5e-da6a-4654-ba4a-513fd50fc830', lineage).
narrative_ontology:cs_reading_relation('d8961e5e-da6a-4654-ba4a-513fd50fc830', ost_article_ii_non_appropriation__extraction_permissive, forecloses).
narrative_ontology:cs_reading_relation('d8961e5e-da6a-4654-ba4a-513fd50fc830', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('d8961e5e-da6a-4654-ba4a-513fd50fc830', foundational, de_facto_extraction_constitutes_appropriation).
narrative_ontology:cs_axiom_status(de_facto_extraction_constitutes_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('d8961e5e-da6a-4654-ba4a-513fd50fc830', de_facto_extraction_constitutes_appropriation, conventional).
narrative_ontology:cs_axiom('d8961e5e-da6a-4654-ba4a-513fd50fc830', foundational, celestial_resources_res_extra_commercium_pending_collective_decision).
narrative_ontology:cs_axiom_status(celestial_resources_res_extra_commercium_pending_collective_decision, holdable).
narrative_ontology:cs_axiom_grounding('d8961e5e-da6a-4654-ba4a-513fd50fc830', celestial_resources_res_extra_commercium_pending_collective_decision, deontological).
narrative_ontology:cs_reference_frame('d8961e5e-da6a-4654-ba4a-513fd50fc830', plain_text_appropriation_prohibition).
narrative_ontology:cs_drift_state('d8961e5e-da6a-4654-ba4a-513fd50fc830', post_cslca_artemis_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('d8961e5e-da6a-4654-ba4a-513fd50fc830', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, scientific_open_access_community).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, asteroid_mining_companies).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, lunar_resource_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, major_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, major_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, artemis_accords_signatories).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, common_heritage_of_mankind_doctrine).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, province_of_all_mankind_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constitute the large majority of treaty parties without independent launch or extraction capability. They preserve a veto over any enclosure of celestial territory or resources through consensus norms, General Assembly positions, and the absence of any recognized unilateral title. They contribute none of the development costs; their return is the option value of an open commons plus bargaining weight in any future benefit-sharing negotiation, exercised only inside multilateral forums they cannot leave.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, beneficiary,
    organized, generational, constrained, global).

% Operate launch, exploration, and prospective resource missions, and bear the wall's restraint costs: mission architectures, site plans, and investment theses must be built around non-appropriation exposure. They simultaneously collect the anti-scramble stability the norm provides — no rival territorial grab triggers a matching grab — and hold agenda power inside the treaty institutions. Their exits are unusually good: domestic statutory reinterpretation, parallel framework-building, and formal treaty withdrawal.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, major_spacefaring_states, payer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__commons_conservation, major_spacefaring_states, beneficiary).

% Hold first-mover capital committed against mission timelines set by venture funding cycles. Under this reading their planned tenure yields no recognized property beyond extracted chattels, and a hardened or enforced wall strands their investment. Their exit is jurisdictional arbitrage: incorporation and licensing in permissive jurisdictions, mission structures built around national grants of extracted-material rights.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, asteroid_mining_companies, payer,
    moderate, immediate, arbitrage, global).

% Prospect water ice and regolith at the lunar poles under national authorization and safety-zone doctrines. The wall renders their planned site tenure insecure — continuous presence does not ripen into title under this reading — so their capital carries a standing legal discount. Exit mirrors jurisdictional arbitrage plus mission redesign toward regimes that recognize their tenure.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, lunar_resource_operators, payer,
    moderate, biographical, arbitrage, regional).

% Relies on guaranteed open access to landing sites, observation windows, and planetary environments for research. The wall protects that access from exclusive claims and exclusionary site control. It bears essentially none of the restraint costs, since it extracts nothing and seeks no title.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, scientific_open_access_community, beneficiary,
    moderate, generational, constrained, global).

% Convenes the standing interpretive forum, manages the space-resources agenda item, and records whatever consensus line survives. It holds no enforcement power — its products are soft law and agenda control — and under this reading it is the body through which any multilateral authorization of extraction would have to pass. It cannot exit its convening role without dissolving the only venue the wall has.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, copuos_legal_subcommittee, agenda_setter,
    institutional, generational, constrained, global).

% States that have joined a parallel framework endorsing national authorization and safety-zone doctrines. Measured against the conservation wall their signature conduct is defection: they decline the restraint the reading assigns them and have already executed the exit by building an alternative institutional track rather than amending or leaving the treaty.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, artemis_accords_signatories, payer,
    institutional, biographical, arbitrage, global).

% Produces the doctrinal mapping of the three readings — restatement projects, manual-length treatments of the resource question, treaty-commentary traditions. Holds no material stake beyond reputation and supplies the external corroboration layer for assessing what the founding problem was and whether it persists.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, space_law_scholarly_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__commons_conservation, diffuse).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__commons_conservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the enclosure-race problem: absent a shared non-appropriation rule, each capable actor has reason to convert celestial sites and resources into exclusive holdings before rivals do, producing conflict risk and foreclosed options for everyone else. The rule holds the commons open pending collective decisions, at lower coercive overhead than a scramble-and-counterclaim equilibrium.
% TRANSFER_FUNCTION: Moves development opportunity and first-mover returns away from capability-holders, who would otherwise appropriate by use, into a collective reserve no seat may unilaterally claim; and moves bargaining leverage to states without capability, who acquire veto and claim power over resources they cannot themselves develop. Any eventual distribution runs through negotiation rather than capability.
% ABSENT_VOICES: Would-be extractors are present but outgunned: industry voices reach the forum only filtered through national delegations, and permissive-reading states are diluted by the consensus process. Structurally absent: future generations, the ultimate holders of the common-heritage claim, have no seat; humanity-as-such appears only through state proxies; and the many states that never accepted the benefit-sharing architecture shape its successor by their absence.
% DISAPPEARANCE_RATIONALE: If the wall vanished overnight, extraction claims would proliferate immediately: licensed operators would extend site control, safety zones would harden into de facto territory, and the non-capable majority would lose its veto without ever having held a substitute. The commons would partition along capability lines within a decade, and the current investment pipeline — built in part on the assumption that open access persists — would repriced around enclosed claims.
% FOUNDING_PROBLEM: Built in the 1958–1967 window to stop the terrestrial scramble from extending upward: sovereign territorial claims, military occupation, and colonial-style acquisition of celestial bodies by whichever powers arrived first.
% FOUNDING_PROBLEM_CORROBORATION: The territorial core is corroborated from outside any benefiting party: the 1967 negotiating record and ratification testimony state the scramble anxiety plainly, all depositary governments maintain uniform non-recognition of sovereignty claims, and neutral scholarship (restatement projects and the McGill manual on space-resource law) confirms the founding problem as anti-appropriation. For the extraction-specific extension, no source outside the benefiting parties attests that the founders intended the wall to reach resource extraction — that extension rests on interpretive argument from ordinary meaning and object-and-purpose, and the permissive camp denies it outright. That absence of corroboration is itself the contest.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__commons_conservation, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__commons_conservation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__commons_conservation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__commons_conservation, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__commons_conservation, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.50: the wall imposes real, uncompensated restraint costs on capability-holders while conferring unearned veto leverage on the non-capable majority — a genuine transfer structure even under this reading's sympathetic lights, which would call the same costs legitimate governance. Suppression 0.58: persistence no longer rides on self-restraint; it requires active interpretive defense against statutory defection and a parallel institutional track, and the enforcement capacity behind that defense is thin. Theater 0.47: COPUOS ritual, reaffirmation resolutions, and common-heritage rhetoric continue at volume while national practice diverges — a growing share of the wall's activity is performative maintenance. Accessibility_collapse 0.35: alternatives do not collapse on understanding the constraint, because the rival reading supplies a live, legislated exit (permissive jurisdictions, accords membership) — the leak is the point. Resistance 0.62: express statutory authorization of extracted-material rights, an expanding accords bloc, and industry advocacy constitute organized, growing resistance. The measurement series run on one shared seven-point grid (every tracked metric authored at every point) so no end-state value is silently substituted into earlier years. The suppression_requirement series is authored deliberately: the story tracks an enforcement ratchet — the wall's suppressive force requirement roughly quadrupled as permissive statutes forced active defense — not merely shifting extraction. Extractiveness rises monotonically because approaching capability makes restraint bite on real plans; theater rises because declaratory activity decouples from compliant practice. No cyclical dynamics: the drift is monotonic, driven by capability maturation, not oscillating enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the engine derives that from structure. From the mining seats the wall is near-pure obstruction: it takes their planned returns and offers no service they can use, with exit available only by fleeing to permissive jurisdictions. From the non-spacefaring bloc seat it is cheap insurance: maximal benefit, zero development cost, veto power as compensation for capability they lack. From the major-spacefaring seat it is genuinely tangled: stability worth having, incidence they resent, exits they increasingly exercise. From the agenda-setter seat it is stewardship awaiting tools. The authored claim does not adjudicate among these; the per-seat computation does.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: non_spacefaring_states and scientific_open_access_community sit near the beneficiary end (low d, damped or inverted effective extraction). Victim declarations drive the opposite pole: asteroid_mining_companies and lunar_resource_operators sit near the target end, but their arbitrage-grade exit damps effective extraction — mobile capital escapes by reincorporation, which is precisely why the wall leaks rather than bites. major_spacefaring_states are dual-positioned (payer with secondary beneficiary role): the derivation tempers their target-side d with their collected stability and agenda power, landing them near symmetric — no override needed because the declared roles plus mobile exit already produce the right relationship. copuos_legal_subcommittee declares no beneficiary/victim position; its directionality comes from the power-atom fallback, and the uncertainty is carried in the veto-leverage omega. No directionality_overrides are authored: the structural data already yields the correct relationships, and the one genuinely ambiguous seat (the agenda setter) is documented as an omega rather than patched with an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two symmetrical errors. Reading the wall as pure rope because over a hundred states acceded ignores that its costs concentrate on capability-holders while its veto-rents concentrate on the non-capable bloc — asymmetric extraction riding a real coordination core, sustained by active interpretive enforcement. Reading it as pure snare because miners complain ignores the genuine anti-enclosure function that even the payers' home governments officially affirm. The scaffold reading fails for lack of any sunset: this reading holds the prohibition operative indefinitely, not transitionally. The founding-problem interview shows the mandate is contested, not dead — the territorial core still operates and is universally complied with — so no resolved-mandatrophy declaration is authored, and the status-x-verdict pair (contested x world_rearranges) raises no zombie flag. One honest tension is recorded rather than smoothed: the receipt surface (diffuse gains, prohibitive fixing) resembles the piton diagnostic cell, but the constraint retains a live coordination function, meets organized resistance, and extracts from identifiable seats — piton's cost-asymmetry test (administrator could change it cheaply but does not) does not describe a wall whose fixing requires treaty-level consensus. The divergence, if the engine weights the cell heavily, is data the corpus exists to take.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_ii_reading_contest,
    'This constraint is the commons_conservation reading of kernel ost_article_ii_non_appropriation. What structurally changes if a sibling reading prevails — extraction_permissive (sovereign claims barred, extracted chattels ownable) or international_regime (question deferred to a future framework)?',
    'Authoritative adjudication — ICJ referral, a binding multilateral resource regime, or coordinated state practice crystallizing one reading as custom.',
    'Under extraction_permissive the victim set dissolves (no stranded-investment wrong; the wall shrinks to a sovereignty-only rule with negligible extraction) and this story''s classification collapses toward rope or mountain-of-text. Under international_regime the present-day classification is suspended entirely — the operative constraint becomes the deferral itself. The disagreement is located in the semantic scope of ''appropriation'' and the settledness of the law now.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_ii_reading_contest, conceptual, 'Committer structure: one of three live readings of the Article II kernel; sibling adoption rewrites victims, epsilon, and type.').

omega_variable(
    customary_law_reach,
    'Does the non-appropriation wall bind non-parties and withdrawing states as customary international law, or only treaty parties subject to the withdrawal clause?',
    'State-practice and opinio-juris analysis: whether non-parties and any withdrawing state are treated as bound, and whether protest or acquiescence patterns crystallize a custom.',
    'If treaty-only, the wall''s reach contracts to its shrinking compliant core and effective extraction on arbitrage seats falls further; if customary, defectors face broader illegitimacy costs and the wall''s suppression requirement drops.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_reach, empirical, 'Whether the wall''s legal reach extends beyond the treaty parties.').

omega_variable(
    enforcement_capacity_fait_accompli,
    'Could the wall actually interdict a completed large-scale extraction — a polar ice-harvesting or platinum-group operation presenting the world with a fait accompli — or is it normative-only?',
    'Coalition-capability analysis: what countermeasures a conservation coalition could impose on an operating extractor (market access, licensing reciprocity, asset measures) versus what the operator''s sponsoring state could absorb.',
    'If enforcement capacity is nil, the wall is performance maintained by ritual — theater_ratio understates decay and the classification drifts toward piton; if credible countermeasures exist, the tangled_rope reading holds with active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_fait_accompli, empirical, 'Whether the wall can survive contact with a determined first extractor.').

omega_variable(
    stranding_realization,
    'How much victimhood is realized versus prospective? No first-mover investment has yet been destroyed by the wall; the stranded-investment harm exists today only as discounted capital and foregone plans.',
    'Track realized losses as missions fly: writedowns attributable to legal insecurity, abandoned programs, litigation costs — versus continued capital inflow betting on the permissive reading.',
    'If the harm stays permanently prospective, victim standing weakens toward foregone-opportunity and the wall''s extractive incidence shifts from taking to denial; if enforcement hardens against flying missions, realized stranding materializes and the victim declarations sharpen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stranding_realization, empirical, 'Whether the declared victims have borne realized losses or only discounted risk.').

omega_variable(
    veto_leverage_vs_gain_capture,
    'Is the non-spacefaring bloc''s veto over enclosure the RECEIPT of the wall''s gains (capture by a named seat) or positional influence that receives nothing the wall withholds?',
    'Trace any concrete benefit-sharing negotiation: if negotiated distributions systematically flow to the bloc as the price of its consent, the veto converts to receipt and gain_flow should name the seat; if consent is given without side-payment, the gains remain diffuse.',
    'This omega documents the authored ''diffuse'' judgment on the receipt surface: the wall seizes nothing and the withheld value stays in the ground, enjoyed as open access by every seat including the payers. Resolution toward named-seat capture would recolor the constraint toward snare-flavored incidence; confirmation of diffuse receipt supports the coordination-core reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_leverage_vs_gain_capture, conceptual, 'Whether bloc veto power constitutes receipt of gains or mere positional leverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__commons_conservation, 1967, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1967, 0.1).
narrative_ontology:measurement_basis(ost__tr_t1967, observed).
narrative_ontology:measurement(ost__tr_t1979, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1979, 0.24).
narrative_ontology:measurement_basis(ost__tr_t1979, observed).
narrative_ontology:measurement(ost__tr_t1991, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1991, 0.31).
narrative_ontology:measurement_basis(ost__tr_t1991, observed).
narrative_ontology:measurement(ost__tr_t2004, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2004, 0.33).
narrative_ontology:measurement_basis(ost__tr_t2004, observed).
narrative_ontology:measurement(ost__tr_t2015, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2015, 0.4).
narrative_ontology:measurement_basis(ost__tr_t2015, observed).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2020, 0.44).
narrative_ontology:measurement_basis(ost__tr_t2020, observed).
narrative_ontology:measurement(ost__tr_t2026, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2026, 0.47).
narrative_ontology:measurement_basis(ost__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1967, 0.18).
narrative_ontology:measurement_basis(ost__be_t1967, observed).
narrative_ontology:measurement(ost__be_t1979, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1979, 0.26).
narrative_ontology:measurement_basis(ost__be_t1979, observed).
narrative_ontology:measurement(ost__be_t1991, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1991, 0.3).
narrative_ontology:measurement_basis(ost__be_t1991, observed).
narrative_ontology:measurement(ost__be_t2004, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2004, 0.34).
narrative_ontology:measurement_basis(ost__be_t2004, observed).
narrative_ontology:measurement(ost__be_t2015, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement_basis(ost__be_t2015, observed).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2020, 0.46).
narrative_ontology:measurement_basis(ost__be_t2020, observed).
narrative_ontology:measurement(ost__be_t2026, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2026, 0.5).
narrative_ontology:measurement_basis(ost__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1967, 0.14).
narrative_ontology:measurement_basis(ost__su_t1967, observed).
narrative_ontology:measurement(ost__su_t1979, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1979, 0.26).
narrative_ontology:measurement_basis(ost__su_t1979, observed).
narrative_ontology:measurement(ost__su_t1991, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1991, 0.29).
narrative_ontology:measurement_basis(ost__su_t1991, observed).
narrative_ontology:measurement(ost__su_t2004, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2004, 0.33).
narrative_ontology:measurement_basis(ost__su_t2004, observed).
narrative_ontology:measurement(ost__su_t2015, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2015, 0.46).
narrative_ontology:measurement_basis(ost__su_t2015, observed).
narrative_ontology:measurement(ost__su_t2020, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2020, 0.53).
narrative_ontology:measurement_basis(ost__su_t2020, observed).
narrative_ontology:measurement(ost__su_t2026, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(ost__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__commons_conservation, resource_allocation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__international_regime).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Article II non-appropriation.' The label conflates three structurally distinct claims with different epsilon, victim sets, and enforcement profiles: this conservation reading (wall binding states and private actors against extraction absent authorization; moderate epsilon; stranded first-movers as victims), the extraction_permissive reading (sovereignty-only bar; negligible extraction on its own terms; miners as beneficiaries), and the international_regime reading (deferral; classification suspended pending a framework). Each is authored as a separate story with its own stable epsilon per the epsilon-invariance principle; all three are linked here. Upstream/downstream structure: the text's settled territorial core — the one element all three readings share — lends borrowed legitimacy to each reading's contested extension, so the family's shared upstream is the treaty text itself, while the Moon Agreement's benefit-sharing architecture stands downstream of this reading as its attempted institutionalization (never ratified by the major spacefaring powers). Contamination propagates across the family: statutory defection under the permissive reading erodes this wall's compliance baseline, and regime-building momentum under the third reading redirects this wall's enforcement demand.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
