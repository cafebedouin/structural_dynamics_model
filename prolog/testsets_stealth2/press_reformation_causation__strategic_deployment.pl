% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__strategic_deployment, []).

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
 *   constraint_id: press_reformation_causation__strategic_deployment
 *   human_readable: Strategic Print Deployment of the Early Reformation (Agency-Centered Reading)
 *   domain: history of technology / religious history / media studies
 *
 * SUMMARY:
 *   Between Luther's theses and the Peace of Augsburg, an arrangement formed
 *   in which reform-minded authors and master printers deliberately converted
 *   print capacity into doctrinal reach, market profit, and political
 *   leverage. This story instantiates the strategic_deployment reading of the
 *   press_reformation_causation kernel: agency sits upstream, the press is
 *   treated as neutral capacity awaiting purposeful use, and the arrangement
 *   is claimed as rope — a coordination tool that dispersed, persecuted
 *   communities used to solve a dissemination problem no manuscript network
 *   could solve at price and speed. The claim/metric gap is deliberate and
 *   load-bearing: the reading CLAIMS clean tool-use coordination, while the
 *   authored metrics describe real friction — concentrated monetary capture
 *   by printers, contested-power transfer imposed on the church, quiet
 *   displacement of scribes, and thirty years of escalating
 *   counter-enforcement. Where the engine's computed type diverges from the
 *   rope claim, that divergence measures the reading's blind spot, not an
 *   authoring error. CONSTRAINT FAMILY (epsilon decomposition of the
 *   colloquial label 'did the press cause the Reformation'): the
 *   natural-language question covers three structurally distinct claims,
 *   authored as three linked stories. The technological_determinism sibling
 *   authors high extraction over an unstoppable-transition arrangement in
 *   which no one chose anything (victims are whoever the wave overran; the
 *   structure trends toward inevitability). The mutual_shaping sibling
 *   authors a co-evolutionary arrangement with no clean winner/loser split
 *   and elevated epsilon from the technology's independent shaping force.
 *   THIS story authors low-moderate epsilon (0.32) over a
 *   deliberate-deployment arrangement with identifiable winners and losers.
 *   Upstream/downstream: the determinist claim is the popular upstream
 *   narrative; this reading arose as its corrective; mutual_shaping
 *   synthesizes against both. All three are linked via
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   evangelical_reformers: Primary beneficiary-agenda-setter
 *   (organized/identity_locked) — convert print capacity into doctrinal
 *   authority and movement coordination - master_printers: Production
 *   gatekeeper and monetary capturer (moderate/constrained) — allocate press
 *   capacity, book the profits - literate_urban_readers: Mass beneficiary
 *   (moderate/mobile) — buy vernacular access, fund the network -
 *   catholic_church_hierarchy: Primary payer (institutional/trapped) — lose
 *   narrative monopoly, fund failed containment - imperial_urban_censors:
 *   Regulatory agenda-setter turned payer (institutional/constrained) —
 *   administer bans the network renders unenforceable - scriptoria_scribes:
 *   Collateral payer (powerless/constrained) — manuscript livelihoods
 *   undercut - historiographers_of_the_book: Analytical observer
 *   (analytical/analytical) — see all three readings of the kernel
 *
 * KEY AGENTS:
 *   - evangelical_reformers: primary beneficiary-agenda-setter (organized/identity_locked) — supply vernacular content, orchestrate publication timing, cannot recant
 *   - master_printers: production gatekeeper and monetary capturer (moderate/constrained) — decide what prints, manage smuggling routes, book pamphlet-boom profits
 *   - literate_urban_readers: mass beneficiary (moderate/mobile) — purchase cheap vernacular access, read aloud, pass copies on
 *   - catholic_church_hierarchy: primary payer (institutional/trapped) — lose controlled-transmission authority, fund escalating failed containment
 *   - imperial_urban_censors: regulatory agenda-setter turned payer (institutional/constrained) — issue mandates whose failure raises the political price of the next
 *   - scriptoria_scribes: collateral payer (powerless/constrained) — commissioned copying contracts as print editions undercut standard texts
 *   - historiographers_of_the_book: analytical observer (analytical/analytical) — hold the seat from which all three kernel readings are visible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, 0.32).
domain_priors:suppression_score(press_reformation_causation__strategic_deployment, 0.3).
domain_priors:theater_ratio(press_reformation_causation__strategic_deployment, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, extractiveness, 0.32).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__strategic_deployment, rope).
narrative_ontology:human_readable(press_reformation_causation__strategic_deployment, "Strategic Print Deployment of the Early Reformation (Agency-Centered Reading)").
narrative_ontology:topic_domain(press_reformation_causation__strategic_deployment, "history of technology / religious history / media studies").

domain_priors:requires_active_enforcement(press_reformation_causation__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__strategic_deployment, '443ca6b3-daa9-42d6-8b75-667f41a49a33').
narrative_ontology:cs_kernel_codification('443ca6b3-daa9-42d6-8b75-667f41a49a33', distributed).
narrative_ontology:cs_authority_grounding('443ca6b3-daa9-42d6-8b75-667f41a49a33', distributed).
narrative_ontology:cs_reading_relation('443ca6b3-daa9-42d6-8b75-667f41a49a33', press_reformation_causation__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('443ca6b3-daa9-42d6-8b75-667f41a49a33', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_axiom('443ca6b3-daa9-42d6-8b75-667f41a49a33', foundational, technological_artifacts_inert_without_agents).
narrative_ontology:cs_axiom_status(technological_artifacts_inert_without_agents, holdable).
narrative_ontology:cs_axiom_grounding('443ca6b3-daa9-42d6-8b75-667f41a49a33', technological_artifacts_inert_without_agents, empirically_contingent).
narrative_ontology:cs_axiom('443ca6b3-daa9-42d6-8b75-667f41a49a33', foundational, deliberate_strategy_drives_media_outcomes).
narrative_ontology:cs_axiom_status(deliberate_strategy_drives_media_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('443ca6b3-daa9-42d6-8b75-667f41a49a33', deliberate_strategy_drives_media_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('443ca6b3-daa9-42d6-8b75-667f41a49a33', agent_centered_instrumental_history).
narrative_ontology:cs_drift_state('443ca6b3-daa9-42d6-8b75-667f41a49a33', post_scot_sts_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('443ca6b3-daa9-42d6-8b75-667f41a49a33', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__strategic_deployment, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, evangelical_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, master_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, literate_urban_readers).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, catholic_church_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, scriptoria_scribes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, imperial_urban_censors).
narrative_ontology:constraint_vindicates(press_reformation_causation__strategic_deployment, vernacular_theology_legitimacy).
narrative_ontology:constraint_vindicates(press_reformation_causation__strategic_deployment, decentralized_information_distribution_feasibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author vernacular treatises, sermons, and polemics and supply them to cooperating print shops; orchestrate publication timing around imperial diets and public disputations. After the 1521 Worms appearance, public recantation would mean spiritual self-destruction under their own published doctrine, so the course is fixed. Income flows from dedications, patronage, and negotiated royalties; the decisive return is doctrinal reach and movement coordination across linguistic borders.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, evangelical_reformers, beneficiary,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, evangelical_reformers, agenda_setter).

% Own the presses, hire compositors, decide which manuscripts become editions, and run smuggling routes when imperial bans close legal channels. Capital is sunk in type, matrices, and inventory; city councils license or harass them depending on confession. Pamphlet-boom profits of the 1520s were exceptional; margins compressed as the market saturated and competitors multiplied. Relocation is possible but costly — presses, equipment, and trained crews must move together.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, master_printers, agenda_setter,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, master_printers, beneficiary).

% Buy pamphlets at prices set for broad sale, read them aloud in households and taverns, and pass copies hand to hand. Coin spent is small; access gained is unprecedented — direct encounter with scripture translation and polemic without clerical mediation. They can stop buying at will; sermons and songs remain free alternatives.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, literate_urban_readers, beneficiary,
    moderate, biographical, mobile, regional).

% Holds doctrinal authority constituted through controlled transmission — pulpit, faculty, and license. Every unauthorized edition bypasses that control. Containment requires banning, burning, indexing, and prosecuting across hundreds of jurisdictions, and each attempt demonstrates the machinery's limits. Retreat from the authority claim itself would dissolve the institution's constitution, so exit is unavailable; the costs arrive as lost obedience, funded counter-printing, and escalating enforcement outlays.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, catholic_church_hierarchy, payer,
    institutional, civilizational, trapped, continental).

% Imperial diets and city councils issue mandates, licensing rules, and confiscation orders; enforcement depends on local officers whose sympathies often lie with the printers. Each failed mandate raises the political price of the next. They administer the counter-regime and absorb its costs while their own constituents trade in the prohibited goods.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, imperial_urban_censors, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, imperial_urban_censors, payer).

% Copy manuscripts for churches, universities, and private clients. Print editions undercut commissioned copying first in standard texts — breviaries, grammars, indulgences — then broadly. Some convert to compositor or proofreader work; older copyists with specialized hands find fewer commissions. Losses accumulate quietly and are thinly recorded.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, scriptoria_scribes, payer,
    powerless, biographical, constrained, regional).

% Study the episode from archives and libraries centuries removed. They hold the analytical seat from which all three causal accounts of the kernel are simultaneously visible, and their adjudications shape which account textbooks and commemorations carry forward.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, historiographers_of_the_book, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__strategic_deployment, master_printers).
narrative_ontology:fixing_cost_class(press_reformation_causation__strategic_deployment, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: how geographically dispersed, legally endangered reform-minded communities receive identical doctrinal content quickly, cheaply, and in the vernacular. Standardized editions, pamphlet formats, and reprint serialization aggregate demand that no manuscript network could serve at comparable price or latency; printers additionally allocate scarce press capacity among competing titles through the market.
% TRANSFER_FUNCTION: Moves coin from readers and patrons to printers and authors; moves attention and doctrinal authority from the clerical transmission hierarchy to pamphlet authors and print shops; moves political leverage to princes and magistrates who align with the reform cause. The church's controlled-transmission position is the stock being drawn down.
% ABSENT_VOICES: Ordinary readers' actual reception is inferred from purchase and possession patterns rather than recorded testimony. Women in the print trades — widows who ran shops after masters' deaths — are largely unnamed in the record. The rural illiterate majority, reached only indirectly through preaching and song, had no seat. Displaced scribes left almost no protest record. And Catholic internal reformers, whose agendas were swept aside by polemical dynamics, would have objected to the terms of the contest itself.
% DISAPPEARANCE_RATIONALE: Remove the print-deployment arrangement around 1520 and the Reformation does not occur on schedule or in its known form: reform ideas remain regional, academic, and Latin; princes lack propaganda cover for territorial conversion; the schism's speed, scale, and vernacular character all depend on the network. Contested across readings — the determinist sibling predicts a substitute medium would have surfaced the same forces, and the substitute-channel omega keeps that challenge live — but within this reading's frame the arrangements visibly depend on it.
% FOUNDING_PROBLEM: Disseminating doctrinal dissent at scale under a hostile information monopoly: manuscript copying was slow, expensive, and censorable at identifiable chokepoints, so dissent could be strangled by seizing a few scripts and silencing a few mouths. Print enabled parallel, redundant, evasive distribution that no chokepoint strategy could close.
% FOUNDING_PROBLEM_CORROBORATION: No benefiting party attests this. Imperial chancery records state the threat authorities believed print posed (the edicts' own preamble language); Catholic counter-print production ledgers attest the perceived stakes from the opposing side; confessionalization-era visitation records and modern book-history scholarship outside the beneficiary set attest that the dissemination crisis closed with the mid-century settlements, after which printers' account books show a pivot to non-polemical genres as polemic margins compressed.
narrative_ontology:disappearance_verdict(press_reformation_causation__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__strategic_deployment, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__strategic_deployment, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__strategic_deployment_tests).
:- end_tests(press_reformation_causation__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.32 because the arrangement's transfers were largely priced into its own aims: reformers wanted reach, printers wanted margin, readers wanted access, and all three received what they paid for. The residual extraction is real but bounded — the church's lost narrative control was the deployment's explicit objective (contested-power transfer, not covert rent), and the scribes' displacement is classic collateral creative destruction. Suppression is authored at 0.30 and is a raw structural property, unscaled: the arrangement's own coercive machinery was light (city licenses, guild norms, doctrinal policing within the movement); the heavy coercion of the era ran in the OPPOSITE direction — imperial and ecclesiastical suppression OF the network — which registers as resistance (0.62), not suppression. Theater is low (0.18): the overwhelming share of activity was functional printing and distribution; the theatrical component (staged disputations, book and bull burnings, the Worms performance, the Augsburg Confession as ceremonial document) served as amplification, not substitute. Accessibility_collapse at 0.50: print added capacity rather than replacing alternatives — manuscript, song, woodcut, and preaching remained viable for many functions — but the economics of mass persuasion shifted irreversibly once pamphlet pricing appeared. Resistance at 0.62 reflects the Edict of Worms, the Speyer protest dynamics, confiscations, and printer prosecutions across the interval. Identity-lock note: the reformer seat is identity_locked by commitment devices of their own making — after 1521, recantation meant spiritual self-destruction under their own published doctrine, and printed permanence raised the cost of reversal for everyone. Same-level note: reformers and printers sat at comparable organizational scale but diverge sharply on exit — printers held movable capital (constrained), reformers held immovable selves (identity_locked) — which differentiates their directionalities despite equal nominal standing. The measurement series run on one shared nine-point grid (1517-1555) so every tracked metric is authored at every examined time point; suppression_requirement is tracked because enforcement-capacity change IS part of this story's arc: internal enforcement hardened through the confessional struggles of the 1530s-40s, peaked around the Augsburg Interim crisis, then relaxed as the 1555 settlement normalized licensed printing. End-state values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the divergence is the finding. From the printer seat the arrangement looks like disciplined market coordination — risky capital, licensed cities, smuggling logistics, fair returns. From the reformer seat it looks like providential instrumentation — a tool seized and aimed. From the church seat the same structure operates as predatory dismantling: a divinely ordered transmission monopoly bypassed by machines its holders never consented to, with containment costs that escalated precisely because exit was unavailable. From the scribe seat it is quiet dispossession without spectacle. The engine computes these per-seat classifications from the structural data; the authored rope claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The three declared beneficiary groups derive low directionality (subsidized seats): reformers collect authority, printers collect margin, readers convert small coin into unprecedented access. Readers sit nearest symmetric — their costs are trivial and voluntary. The church hierarchy derives high directionality (near full-target): it bears the arrangement's principal imposed cost, and its trapped exit amplifies effective extraction — it cannot abandon the authority claim without dissolving the institution. Scribes derive high directionality but hold little power, so their extraction is diffuse and poorly recorded. The censor seat is genuinely dual: it administers the counter-regime (agenda-setting over the arrangement's operating environment) while absorbing that regime's escalating costs, which is why it carries agenda_setter with a payer secondary role. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already place every seat correctly, and the derivation chain handles the dual-positioned agents through secondary roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim guards against two mislabelings at once. Against the determinist error: reading the arrangement as technological destiny erases the deliberate choices — editorial, financial, logistical — that made it work, and would misclassify a chosen coordination tool as an impersonal force. Against the snare error: reading the arrangement as pure predation misses that its coordination function was genuine and its transfers were largely consensual and priced; the church's loss was the declared objective of a contest, not a hidden skim. On the genealogy: the founding problem — disseminating doctrinal dissent under a hostile information monopoly with censorable chokepoints — was real, and it died with the confessional settlement: after 1555 the arrangement dissolved into ordinary commercial print capitalism rather than persisting as a zombie mandate. The mandatrophy risk therefore lives NOT in the historical arrangement but in the historiographical settlement that narrates it: the strategic-deployment story persists in textbooks long after its object closed, and the mismatch consumer (founding_problem_status=dead x world_rearranges) flags exactly that residue for the meta-level.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the press_reformation_causation kernel — the strategic_deployment reading. What structural changes follow if a sibling reading is adopted instead?',
    'Adopting the technological_determinism sibling removes agent-level beneficiary/victim structure entirely: outcomes are attributed to press capacity, victims become whoever the transition crushed regardless of anyone''s choice, and the classification trends toward inevitability-flavored structure with snare-flavored seats for the overrun. Adopting the mutual_shaping sibling dissolves the clean winner/loser split into co-constituted positions and raises epsilon by crediting the technology with independent shaping force.',
    'Classification, beneficiary/victim structure, and epsilon change wholesale under sibling adoption; the three stories form a linked family via network.affects_constraints and none of them can be merged without violating epsilon-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story instantiates the strategic_deployment reading of a three-reading kernel.').

omega_variable(
    neutrality_vs_affordance_pressure,
    'Is ''neutral capacity awaiting purposeful use'' empirically tenable, or do print''s affordances (fixity, standardization, recombinability, seriality) shape outcomes independently of deployer intent?',
    'Compare diffusion fidelity and content mutation of identical doctrinal messages transmitted through manuscript versus print channels; systematic affordance-driven divergence would falsify strict neutrality.',
    'If affordances materially shaped outcomes, epsilon rises above the authored 0.32 because unintended extraction enters the arrangement, and the reading drifts toward the mutual_shaping sibling; the foundational axiom technological_artifacts_inert_without_agents erodes under its empirically_contingent grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_vs_affordance_pressure, empirical, 'Whether the reading''s neutrality premise survives affordance-level evidence.').

omega_variable(
    conviction_vs_margin_selection,
    'Did printers select titles by doctrinal conviction or by margin, and how much reader attention was captured through sensational polemic that conviction alone would not have published?',
    'Printer account books, edition run lengths, and price series cross-checked against the doctrinal significance of titles.',
    'Margin-dominated selection would add a hidden extraction layer on readers, pushing the computed type from rope toward tangled_rope and concentrating gain_flow further on master_printers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conviction_vs_margin_selection, empirical, 'Whether the print market''s selection mechanism carried concealed extraction.').

omega_variable(
    scribe_displacement_magnitude,
    'How large were manuscript-trade losses attributable to the print-deployment arrangement?',
    'Guild rolls, scriptoria closure dating, and scribal wage series across 1500-1560.',
    'Large displacement raises victim-side effective extraction and strengthens a tangled_rope computation; negligible displacement supports the rope claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scribe_displacement_magnitude, empirical, 'Magnitude of collateral economic damage to the manuscript trades.').

omega_variable(
    substitute_channel_counterfactual,
    'Absent print, would reform messaging have scaled through song, woodcut, and sermon networks to comparable effect?',
    'Model diffusion rates of pre-print reform movements (Waldensian, Hussite) and of contemporaneous print-independent channels such as broadsheet ballads.',
    'Strong substitutes would lower the arrangement''s indispensability, weakening the world_rearranges verdict and lending support to the determinist sibling''s claim that the medium mattered less than the moment; weak substitutes confirm the deployment reading''s coordination centrality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substitute_channel_counterfactual, empirical, 'Counterfactual indispensability of the print-deployment arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__strategic_deployment, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(strategic_deployment_press_tr_t1517, press_reformation_causation__strategic_deployment, theater_ratio, 1517, 0.1).
narrative_ontology:measurement_basis(strategic_deployment_press_tr_t1517, observed).
narrative_ontology:measurement(strategic_deployment_press_tr_t1521, press_reformation_causation__strategic_deployment, theater_ratio, 1521, 0.16).
narrative_ontology:measurement_basis(strategic_deployment_press_tr_t1521, observed).
narrative_ontology:measurement(strategic_deployment_press_tr_t1524, press_reformation_causation__strategic_deployment, theater_ratio, 1524, 0.14).
narrative_ontology:measurement_basis(strategic_deployment_press_tr_t1524, observed).
narrative_ontology:measurement(strategic_deployment_press_tr_t1529, press_reformation_causation__strategic_deployment, theater_ratio, 1529, 0.18).
narrative_ontology:measurement_basis(strategic_deployment_press_tr_t1529, observed).
narrative_ontology:measurement(strategic_deployment_press_tr_t1534, press_reformation_causation__strategic_deployment, theater_ratio, 1534, 0.17).
narrative_ontology:measurement_basis(strategic_deployment_press_tr_t1534, observed).
narrative_ontology:measurement(strategic_deployment_press_tr_t1540, press_reformation_causation__strategic_deployment, theater_ratio, 1540, 0.19).
narrative_ontology:measurement_basis(strategic_deployment_press_tr_t1540, observed).
narrative_ontology:measurement(strategic_deployment_press_tr_t1546, press_reformation_causation__strategic_deployment, theater_ratio, 1546, 0.21).
narrative_ontology:measurement_basis(strategic_deployment_press_tr_t1546, observed).
narrative_ontology:measurement(strategic_deployment_press_tr_t1550, press_reformation_causation__strategic_deployment, theater_ratio, 1550, 0.2).
narrative_ontology:measurement_basis(strategic_deployment_press_tr_t1550, observed).
narrative_ontology:measurement(strategic_deployment_press_tr_t1555, press_reformation_causation__strategic_deployment, theater_ratio, 1555, 0.18).
narrative_ontology:measurement_basis(strategic_deployment_press_tr_t1555, observed).

% Extraction over time
narrative_ontology:measurement(strategic_deployment_press_be_t1517, press_reformation_causation__strategic_deployment, base_extractiveness, 1517, 0.22).
narrative_ontology:measurement_basis(strategic_deployment_press_be_t1517, observed).
narrative_ontology:measurement(strategic_deployment_press_be_t1521, press_reformation_causation__strategic_deployment, base_extractiveness, 1521, 0.28).
narrative_ontology:measurement_basis(strategic_deployment_press_be_t1521, observed).
narrative_ontology:measurement(strategic_deployment_press_be_t1524, press_reformation_causation__strategic_deployment, base_extractiveness, 1524, 0.33).
narrative_ontology:measurement_basis(strategic_deployment_press_be_t1524, observed).
narrative_ontology:measurement(strategic_deployment_press_be_t1529, press_reformation_causation__strategic_deployment, base_extractiveness, 1529, 0.36).
narrative_ontology:measurement_basis(strategic_deployment_press_be_t1529, observed).
narrative_ontology:measurement(strategic_deployment_press_be_t1534, press_reformation_causation__strategic_deployment, base_extractiveness, 1534, 0.38).
narrative_ontology:measurement_basis(strategic_deployment_press_be_t1534, observed).
narrative_ontology:measurement(strategic_deployment_press_be_t1540, press_reformation_causation__strategic_deployment, base_extractiveness, 1540, 0.4).
narrative_ontology:measurement_basis(strategic_deployment_press_be_t1540, observed).
narrative_ontology:measurement(strategic_deployment_press_be_t1546, press_reformation_causation__strategic_deployment, base_extractiveness, 1546, 0.42).
narrative_ontology:measurement_basis(strategic_deployment_press_be_t1546, observed).
narrative_ontology:measurement(strategic_deployment_press_be_t1550, press_reformation_causation__strategic_deployment, base_extractiveness, 1550, 0.38).
narrative_ontology:measurement_basis(strategic_deployment_press_be_t1550, observed).
narrative_ontology:measurement(strategic_deployment_press_be_t1555, press_reformation_causation__strategic_deployment, base_extractiveness, 1555, 0.32).
narrative_ontology:measurement_basis(strategic_deployment_press_be_t1555, observed).

% Suppression requirement over time
narrative_ontology:measurement(strategic_deployment_press_su_t1517, press_reformation_causation__strategic_deployment, suppression_requirement, 1517, 0.15).
narrative_ontology:measurement_basis(strategic_deployment_press_su_t1517, observed).
narrative_ontology:measurement(strategic_deployment_press_su_t1521, press_reformation_causation__strategic_deployment, suppression_requirement, 1521, 0.22).
narrative_ontology:measurement_basis(strategic_deployment_press_su_t1521, observed).
narrative_ontology:measurement(strategic_deployment_press_su_t1524, press_reformation_causation__strategic_deployment, suppression_requirement, 1524, 0.28).
narrative_ontology:measurement_basis(strategic_deployment_press_su_t1524, observed).
narrative_ontology:measurement(strategic_deployment_press_su_t1529, press_reformation_causation__strategic_deployment, suppression_requirement, 1529, 0.34).
narrative_ontology:measurement_basis(strategic_deployment_press_su_t1529, observed).
narrative_ontology:measurement(strategic_deployment_press_su_t1534, press_reformation_causation__strategic_deployment, suppression_requirement, 1534, 0.38).
narrative_ontology:measurement_basis(strategic_deployment_press_su_t1534, observed).
narrative_ontology:measurement(strategic_deployment_press_su_t1540, press_reformation_causation__strategic_deployment, suppression_requirement, 1540, 0.42).
narrative_ontology:measurement_basis(strategic_deployment_press_su_t1540, observed).
narrative_ontology:measurement(strategic_deployment_press_su_t1546, press_reformation_causation__strategic_deployment, suppression_requirement, 1546, 0.46).
narrative_ontology:measurement_basis(strategic_deployment_press_su_t1546, observed).
narrative_ontology:measurement(strategic_deployment_press_su_t1550, press_reformation_causation__strategic_deployment, suppression_requirement, 1550, 0.4).
narrative_ontology:measurement_basis(strategic_deployment_press_su_t1550, observed).
narrative_ontology:measurement(strategic_deployment_press_su_t1555, press_reformation_causation__strategic_deployment, suppression_requirement, 1555, 0.3).
narrative_ontology:measurement_basis(strategic_deployment_press_su_t1555, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__strategic_deployment, resource_allocation).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the press and the Reformation' conflates three structurally distinct claims with different epsilon values, different beneficiary/victim structures, and different failure modes. This story (strategic_deployment) authors epsilon 0.32 over a deliberate-deployment arrangement; the technological_determinism sibling authors high extraction over an inevitability arrangement with no agent-level beneficiary structure; the mutual_shaping sibling authors a co-evolutionary arrangement with dissolved winner/loser splits. The determinist claim is the popular upstream narrative that this reading corrects; mutual_shaping synthesizes against both. Each member links the others via network.affects_constraints; none may be merged without violating epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
