% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__demographic_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__demographic_trap_reading, []).

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
 *   constraint_id: lycurgan_laws__demographic_trap_reading
 *   human_readable: Lycurgan Constitutional Immutability — Demographic Trap Reading
 *   domain: political philosophy/constitutional theory/commitment systems
 *
 * SUMMARY:
 *   This file instantiates the demographic_trap_reading of the lycurgan_laws
 *   kernel. The standing arrangement under contest is the Great Rhetra's
 *   settlement as rendered unamendable: the rider that transferred effective
 *   amendment power from the assembly to the council and kings, the perpetual
 *   oath sworn to the departed lawgiver, and the institutional machinery —
 *   life-tenured elders, annual overseers, status courts — that guarded the
 *   text instead of governing the population. This reading's claim: the
 *   freeze converted ordinary demographic shocks (war losses, the earthquake
 *   of 464, inheritance dynamics) into a cumulative death spiral, because
 *   every corrective lever — land redistribution, debt relief, citizenship
 *   enlargement, mess-requirement adjustment — required touching the
 *   untouchable text. The citizen roll fell from roughly nine thousand at the
 *   post-Messenian peak to under fifteen hundred by the battle of Leuctra;
 *   the polis that dominated Greece in 480 could field a fragment of its old
 *   phalanx a century later. Per Rule 1, the sibling readings (sacral
 *   fidelity, adaptive fiction) are other constraints in other files; this
 *   story hedges nothing across them and authors epsilon for the standing
 *   arrangement as the trap reading sees it. The claim/metric pair is
 *   independent by design: claimed_type snare is stated from this reading's
 *   structural analysis, and the metrics are authored descriptively from the
 *   historical record. Interval mapping: T=0 approximates 600 BC (peak
 *   rolls), T=40 approximates 222 BC (Sellasia, where foreign armies finally
 *   broke what internal revision never could); one unit is roughly nine and a
 *   half years.
 *
 * KEY AGENTS:
 *   - gerousia_elders: agenda-setter (institutional/identity_locked) — administers the frozen text, collects authority from its permanence
 *   - ephorate_magistrates: enforcement arm (institutional/immediate horizon) — annual officers executing conformity, structurally blind to deferred costs
 *   - landed_elite_families: primary beneficiary (powerful/arbitrage) — consolidate allotments under the freeze's protection
 *   - spartan_women: dual-positioned (moderate/constrained) — property gain without political voice, sons lost to the system
 *   - spartiate_smallholders: primary target (moderate/trapped) — fall below the mess threshold and out of citizenship
 *   - hypomeiones_and_mothakes: locked-out victims (powerless/trapped) — the growing residue of the shrinking citizen body
 *   - helot_underclass: substrate victims (powerless/trapped) — no legislative route to status change ever opens
 *   - royal_reformers: revision-attempters destroyed by enforcement (institutional/trapped)
 *   - peripatetic_analysts: analytical observer — sees the causal loop from outside the benefiting order
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, 0.78).
domain_priors:suppression_score(lycurgan_laws__demographic_trap_reading, 0.86).
domain_priors:theater_ratio(lycurgan_laws__demographic_trap_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__demographic_trap_reading, snare).
narrative_ontology:human_readable(lycurgan_laws__demographic_trap_reading, "Lycurgan Constitutional Immutability — Demographic Trap Reading").
narrative_ontology:topic_domain(lycurgan_laws__demographic_trap_reading, "political philosophy/constitutional theory/commitment systems").

domain_priors:requires_active_enforcement(lycurgan_laws__demographic_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__demographic_trap_reading, '1eba21b6-08e4-4c4b-8a2a-4dcb90c8bc9a').
narrative_ontology:cs_kernel_codification('1eba21b6-08e4-4c4b-8a2a-4dcb90c8bc9a', fixed_text).
narrative_ontology:cs_authority_grounding('1eba21b6-08e4-4c4b-8a2a-4dcb90c8bc9a', lineage).
narrative_ontology:cs_interpretation_layer_present('1eba21b6-08e4-4c4b-8a2a-4dcb90c8bc9a').
narrative_ontology:cs_reading_relation('1eba21b6-08e4-4c4b-8a2a-4dcb90c8bc9a', lycurgan_laws__sacral_fidelity_reading, influences).
narrative_ontology:cs_reading_relation('1eba21b6-08e4-4c4b-8a2a-4dcb90c8bc9a', lycurgan_laws__adaptive_fiction_reading, coexists_with).
narrative_ontology:cs_axiom('1eba21b6-08e4-4c4b-8a2a-4dcb90c8bc9a', foundational, immutability_itself_was_the_failure_mode).
narrative_ontology:cs_axiom_status(immutability_itself_was_the_failure_mode, holdable).
narrative_ontology:cs_axiom_grounding('1eba21b6-08e4-4c4b-8a2a-4dcb90c8bc9a', immutability_itself_was_the_failure_mode, empirically_contingent).
narrative_ontology:cs_axiom('1eba21b6-08e4-4c4b-8a2a-4dcb90c8bc9a', secondary, demographic_outcomes_override_ancestral_authority).
narrative_ontology:cs_axiom_status(demographic_outcomes_override_ancestral_authority, holdable).
narrative_ontology:cs_axiom_grounding('1eba21b6-08e4-4c4b-8a2a-4dcb90c8bc9a', demographic_outcomes_override_ancestral_authority, instrumental).
narrative_ontology:cs_reference_frame('1eba21b6-08e4-4c4b-8a2a-4dcb90c8bc9a', immutable_founder_settlement).
narrative_ontology:cs_drift_state('1eba21b6-08e4-4c4b-8a2a-4dcb90c8bc9a', late_classical_oliganthropia, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('1eba21b6-08e4-4c4b-8a2a-4dcb90c8bc9a', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__demographic_trap_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, gerousia_elders).
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, landed_elite_families).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, spartiate_smallholders).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, hypomeiones_and_mothakes).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, helot_underclass).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, royal_reformers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, spartan_women).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, spartan_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Twenty-eight elders serving for life alongside the two kings. They control what reaches the assembly through the rider's power to set aside crooked decisions, judge deviations from the ancestral forms, and block every proposal that touches the founding text. Their authority exists only inside the frozen order — guardianship of the unchangeable is the office itself; abandoning the freeze would dissolve the basis of their own standing.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, gerousia_elders, agenda_setter,
    institutional, generational, identity_locked, national).

% Five annually elected overseers who administer the oath regime, supervise the kings, prosecute deviants, and enforce conformity of training, dress, and conduct. Their one-year term means the long-run demographic consequences of the freeze are structurally nobody's office's business; each cohort inherits the enforcement duty without owning any of its deferred costs.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, ephorate_magistrates, agenda_setter,
    institutional, immediate, identity_locked, national).

% Old houses that accumulate allotments through heiress marriage and female inheritance while the formal equality of the peers is maintained in name. The freeze is their asset-protection regime: redistribution, debt relief, and citizenship enlargement are precisely the revisions the order forbids. When Agis IV proposes cancelling debts and re-dividing land, he is proposing to take back exactly what they have consolidated.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, landed_elite_families, beneficiary,
    powerful, generational, arbitrage, national).

% Run estates and inherit land on a scale unusual in Greece — Aristotle credits them with two-fifths of the territory — because the men live in barracks and die in campaigns. They gain property standing the frozen order permits, yet vote nowhere in the assembly that preserves the rules, and surrender sons to the training system and to wars that thin the citizen rolls their property depends on.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, spartan_women, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__demographic_trap_reading, spartan_women, payer).

% Owe fixed contributions to the common mess from allotments that shrink with each inheritance split and each campaign season lost. A bad harvest or an heir's minority drops them below the mess threshold and out of citizenship. There is no selling up and leaving: citizenship is the whole legal person, and the class visibly thins at every generation's review.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, spartiate_smallholders, payer,
    moderate, biographical, trapped, national).

% The fallen and the never-admitted: families that once held citizenship and missed a mess payment, sons of Spartiate fathers and helot mothers, fostered retainers who fight in the phalanx without the rights their fathers held. The frozen membership rule offers no path back in, ever; their numbers grow exactly as the citizen roll shrinks.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, hypomeiones_and_mothakes, payer,
    powerless, biographical, trapped, national).

% Bound agricultural workers whose surplus feeds the mess system that defines citizenship. The freeze means no legislative route to manumission or status change ever opens — unlike dependent populations elsewhere in Greece whose arrangements evolved. The only policy instruments ever applied to them are terror: the secret police service, wartime mass enrollments, periodic killings.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, helot_underclass, payer,
    powerless, generational, trapped, regional).

% Kings and their allies who propose restoring the founding equality — debt cancellation, land re-division, re-enrollment of the fallen. The first great attempt ends with the reformer king condemned and executed by his fellow magistrates and his mother and grandmother strangled; a later king revives the program by revolutionary force and is broken only by foreign league armies. The enforcement machinery treats revision-attempts as the enemy it exists to destroy.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, royal_reformers, payer,
    institutional, generational, trapped, national).

% Outside analysts — the Aristotelian school writing from Athens — who diagnose the manpower shortage as a product of the property regime, the neglect of birthrates, and the refusal to enlarge citizenship. They see the whole causal loop because they bear none of its costs and owe nothing to the founder's authority.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, peripatetic_analysts, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__demographic_trap_reading, landed_elite_families).
narrative_ontology:fixing_cost_class(lycurgan_laws__demographic_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Held the citizen body to a single intergenerational settlement — equal allotments, common messes, one training system — so that every peer's expectations about land, rank, and duty were fixed for life and across generations. The immutability commitment itself solved a bargaining problem: no coalition ever had to renegotiate the terms, and every member could rely on the rules not moving.
% TRANSFER_FUNCTION: Moves land and status upward over time — allotments concentrating into fewer hands through heiress marriage and female inheritance — moves fallen families downward out of citizenship, and moves the polity's adaptive capacity, its ability to re-cut membership and property rules as conditions change, from every future generation to the founding settlement.
% ABSENT_VOICES: The hypomeiones and mothakes who had already fallen out, the helots who feed the system, and — decisively — future generations: the unborn Spartiates whose numbers the frozen rules would consume had no seat anywhere in the assembly. Spartan women, whose property practices drive the concentration, vote nowhere.
% DISAPPEARANCE_RATIONALE: Had the amendment power stayed with the assembly, land redistribution, debt relief, and citizenship enlargement were all live options other poleis used routinely. When reformers finally forced the question, the brief revolutionary restoration showed the world rearranging — new citizens enrolled, land re-divided, debts cancelled — before the leagues crushed it. Landholding, mess economics, and military manpower arrangements all depended on the freeze holding.
% FOUNDING_PROBLEM: Internal stasis: the tradition records the lawgiver returning to a city torn between rich and poor. Equal allotments, common tables, and fixed ranks were built to purchase civic concord and military durability by taking distribution off the political agenda permanently.
% FOUNDING_PROBLEM_CORROBORATION: Aristotle, writing from outside the system, attributes the decline directly to the frozen property and membership rules; later external historians treat the rigidity as the recognized cause; the reformer-kings' own programs attest that the founding equality was gone. No seat inside the benefiting order ever conceded the founding problem had died — corroboration comes entirely from outside it.
narrative_ontology:disappearance_verdict(lycurgan_laws__demographic_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__demographic_trap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__demographic_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lycurgan_laws__demographic_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__demographic_trap_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__demographic_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__demographic_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.78: the arrangement's extraction was not money but demographic and adaptive capital — it consumed the citizen body itself while visible decline accumulated for a century with zero structural response, because response required revision. Suppression 0.86 is the load-bearing figure: persistence depended on suppressing the exit called amendment — the rider nullifying assembly sovereignty, status courts, atimia, and ultimately lethal treatment of reformers. Suppression here is both structural (courts, oath machinery, the rider) and internalized (agoge-formed identity made deviation socially dead before any magistrate acted); the split is carried in the internalized_suppression_share omega. Theater ratio 0.32: enforcement was real and the system genuinely delivered military primacy for generations, but a growing share of activity was sacral legitimation — invoking the founder's authority to block change — and the series shows that share rising as function decayed. Accessibility collapse 0.62: revisable constitutional orders were visible across Greece, so alternatives did not vanish from knowledge, but they were internally inaccessible and exit meant total civic annihilation — partial collapse, characteristic of a construct rather than a natural law. Resistance 0.58: continuous low-grade evasion (inheritance strategy, the trembler phenomenon) culminating in open reform movements, each crushed. The measurement series run on one shared nine-point grid so every metric is authored at every examined time point; trajectories are monotonic rises, not cycles — the trap tightened rather than oscillated.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute radically different constraints from identical structural facts. From the gerousia's position the freeze is the constitution's glory — their authority IS the text's permanence, and every blocked reform confirms their guardianship. From the smallholder's position the same structure is a machine that strips citizenship from a man who misses a dinner contribution. The ephorate experiences it as duty without ownership: a one-year office enforcing costs that mature on a century timescale. The engine computes this per-seat divergence from power, exit, and directionality data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure drives the derivation: gerousia_elders and landed_elite_families sit at the beneficiary end (the freeze subsidizes them — authority and land accrue under it); spartiate_smallholders, hypomeiones_and_mothakes, helot_underclass, and royal_reformers sit at the target end (they pay in status, labor, blood, and — for the reformers — life, all with trapped exit, which pushes them toward the full-target pole). The ephorate administers without materially collecting; its income is legitimacy, placing it mildly beneficiary-side. One override is authored: the moderate power atom blends two genuinely different seats — spartan_women (property beneficiaries, politically excluded) and spartiate_smallholders (erosion targets) — which the binary beneficiary/victim derivation cannot split; d=0.58 places the blended moderate seat slightly target-side of symmetric, reflecting that the class-level flow at moderate power runs net outward. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and the national scope's verification difficulty.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is preventing a rope mislabel. The Lycurgan order genuinely solved a coordination problem — it bought civic concord and military durability, and for two centuries it delivered. A naive lifecycle read would call it a rope that aged. The trap reading's data show something else: the founding problem (concord through frozen equality) died — the equality eroded into concentrated landholdings while the form persisted — and the arrangement did not merely outlive its function, it actively consumed the polity that hosted it, with rising extraction and rising enforcement intensity right up to external destruction. Dead founding problem plus world_rearranges verdict plus a named capturing seat is the capture/zombie signature, not benign persistence. Mandatrophy resolution therefore runs through the R5 interview rather than a sunset clause: there was no sunset, no transition, and no internal exit — only foreign armies at Sellasia ended what no assembly could.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates one reading (demographic_trap) of the lycurgan_laws kernel; how would epsilon and classification shift under the sibling readings instantiated as separate constraints?',
    'Author sacral_fidelity_reading and adaptive_fiction_reading as separate stories over the same standing arrangement; compare computed types across the family.',
    'The sacral reading authors low epsilon over the identical referent (adherence as duty, not extraction) and plausibly computes rope-flavored; the fiction reading authors low-to-moderate epsilon (the freeze was never actually binding). Only the family comparison separates reading-indexed assessment from any topic-level fact about Sparta.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one kernel, three readings, epsilon is reading-indexed over a shared referent.').

omega_variable(
    bindingness_of_the_freeze,
    'Was the immutability commitment actually binding on Spartan practice, or did covert adaptation (neodamodeis enrollment, mothax integration, tactical and naval improvisation) keep the system flexible enough that the freeze was never the binding constraint?',
    'Institutional-history audit: catalog every post-rhetra structural change and classify each as formal amendment (expected count: zero), interpretive absorption, or practice-level innovation; test whether any demographic corrective ever reached the membership or property core.',
    'If covert adaptation sufficed, this reading''s epsilon drops toward the fiction reading''s and the snare verdict softens toward rope; if nothing touched the core, the trap reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bindingness_of_the_freeze, empirical, 'Whether the freeze bound practice or was masked by marginal adaptation.').

omega_variable(
    spartiate_enrollment_uncertainty,
    'How reliable are the Spartiate population figures carrying the collapse curve (Herodotean ~8,000-9,000 at the peak; Aristotle''s report of under 1,000 citizens; Leuctra field strengths around 700-1,500)?',
    'Cross-check literary figures against archaeological survey of Laconian and Messenian rural sites, army-logistics reconstructions, and inscription/proxeny counts.',
    'A shallower true curve weakens severity attribution to the freeze (slow decline looks like ordinary drift); a confirmed steep curve strengthens the snare reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spartiate_enrollment_uncertainty, empirical, 'Ancient demographic figures are contested; the curve''s steepness is load-bearing.').

omega_variable(
    external_shock_vs_freeze_attribution,
    'How much of the decline traces to the freeze itself versus shocks any polis suffered — war casualties across a century of hegemonic campaigning, the 464 BC earthquake, the loss of Messenian labor after 369?',
    'Comparative-poleis analysis: Athens lost comparable citizen cohorts in the Peloponnesian War and rebuilt its rolls within a generation under revisable institutions; test whether Sparta''s divergence tracks shock exposure or remedy-blockage.',
    'If shocks suffice, the constraint sits closer to a tragic circumstance-mountain; if remedy-blockage explains the divergence, the snare reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_shock_vs_freeze_attribution, empirical, 'Cause attribution between the freeze and exogenous demographic shocks.').

omega_variable(
    counterfactual_damos_amendment_power,
    'Had the rider never transferred effective amendment power from the damos to the gerousia and kings, would Sparta have adapted demographically?',
    'Counterfactual institutional analysis grounded in the apella''s recorded behavior whenever it retained initiative — it repeatedly favored redistribution-adjacent measures when permitted to decide.',
    'A credible yes makes the rider — a single procedural seizure — the load-bearing extraction device and sharpens the snare verdict; a credible no relocates the trap deeper into the kleros and agoge design.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_damos_amendment_power, conceptual, 'Counterfactual on the procedural hinge of the whole arrangement.').

omega_variable(
    internalized_suppression_share,
    'What share of the measured suppression was structural enforcement (ephoral courts, atimia, the rider) versus internalized identity (agoge-formed selves and the founder-oath making deviation unthinkable before any magistrate acted)?',
    'Compare deviance rates under varying enforcement capacity: after Leuctra the enforcement machinery weakened while much conformity held — the trembler phenomenon and post-collapse Spartan diaspora identity serve as natural experiments for estimating the internalized share.',
    'A high internalized share raises effective suppression above the structural measure and predicts persistence beyond institutional death — Spartan identity visibly outlived the constitution in mercenary companies and nostalgic revival movements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_share, empirical, 'Structural versus internalized components of the suppression metric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__demographic_trap_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycurgan_demographic_trap_tr_t0, lycurgan_laws__demographic_trap_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(lycurgan_demographic_trap_tr_t5, lycurgan_laws__demographic_trap_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(lycurgan_demographic_trap_tr_t10, lycurgan_laws__demographic_trap_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(lycurgan_demographic_trap_tr_t15, lycurgan_laws__demographic_trap_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(lycurgan_demographic_trap_tr_t20, lycurgan_laws__demographic_trap_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(lycurgan_demographic_trap_tr_t25, lycurgan_laws__demographic_trap_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(lycurgan_demographic_trap_tr_t30, lycurgan_laws__demographic_trap_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(lycurgan_demographic_trap_tr_t35, lycurgan_laws__demographic_trap_reading, theater_ratio, 35, 0.31).
narrative_ontology:measurement(lycurgan_demographic_trap_tr_t40, lycurgan_laws__demographic_trap_reading, theater_ratio, 40, 0.32).

% Extraction over time
narrative_ontology:measurement(lycurgan_demographic_trap_be_t0, lycurgan_laws__demographic_trap_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lycurgan_demographic_trap_be_t5, lycurgan_laws__demographic_trap_reading, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(lycurgan_demographic_trap_be_t10, lycurgan_laws__demographic_trap_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(lycurgan_demographic_trap_be_t15, lycurgan_laws__demographic_trap_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(lycurgan_demographic_trap_be_t20, lycurgan_laws__demographic_trap_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(lycurgan_demographic_trap_be_t25, lycurgan_laws__demographic_trap_reading, base_extractiveness, 25, 0.69).
narrative_ontology:measurement(lycurgan_demographic_trap_be_t30, lycurgan_laws__demographic_trap_reading, base_extractiveness, 30, 0.73).
narrative_ontology:measurement(lycurgan_demographic_trap_be_t35, lycurgan_laws__demographic_trap_reading, base_extractiveness, 35, 0.76).
narrative_ontology:measurement(lycurgan_demographic_trap_be_t40, lycurgan_laws__demographic_trap_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(lycurgan_demographic_trap_su_t0, lycurgan_laws__demographic_trap_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(lycurgan_demographic_trap_su_t5, lycurgan_laws__demographic_trap_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(lycurgan_demographic_trap_su_t10, lycurgan_laws__demographic_trap_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(lycurgan_demographic_trap_su_t15, lycurgan_laws__demographic_trap_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(lycurgan_demographic_trap_su_t20, lycurgan_laws__demographic_trap_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(lycurgan_demographic_trap_su_t25, lycurgan_laws__demographic_trap_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(lycurgan_demographic_trap_su_t30, lycurgan_laws__demographic_trap_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(lycurgan_demographic_trap_su_t35, lycurgan_laws__demographic_trap_reading, suppression_requirement, 35, 0.83).
narrative_ontology:measurement(lycurgan_demographic_trap_su_t40, lycurgan_laws__demographic_trap_reading, suppression_requirement, 40, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__demographic_trap_reading, identity_coordination).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the lycurgan_laws kernel. The colloquial label 'the Lycurgan constitution' covers three structurally distinct claims: sacral_fidelity_reading assesses the standing arrangement as divine ordinance (low epsilon, adherence as duty); adaptive_fiction_reading assesses it as a noble lie stretched over covert flexibility (low-to-moderate epsilon, the freeze not actually binding); this file assesses it as a binding freeze that consumed the citizen body (high epsilon, snare). Same referent, reading-indexed epsilon, so each is authored separately and linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lycurgan_laws__demographic_trap_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
