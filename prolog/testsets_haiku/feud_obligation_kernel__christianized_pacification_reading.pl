% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__christianized_pacification_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Blood-Feud Prohibition via Divine Law and Ecclesiastical Authority
 *   domain: legal/religious/political
 *
 * SUMMARY:
 *   The christianized-pacification reading of blood-feud obligations frames
 *   them as violations of divine law (the prohibition on vengeance) and
 *   asserts that legitimate violence authority derives solely from God and
 *   flows through ecclesiastical and royal institutions. This reading emerges
 *   in medieval Christian theology and political consolidation: the Church
 *   teaches that kin-group blood vengeance is sinful and that only anointed
 *   monarchs and ecclesiastical authorities can legitimately exercise
 *   violence. The constraint operates as a tangled rope in this reading: it
 *   coordinates a centralized judgment function (replacing fragmented kinship
 *   adjudication) AND extracts massive authority and compliance from
 *   feud-obligated populations who face spiritual peril (excommunication,
 *   damnation) and legal punishment for maintaining their traditional
 *   obligations. The claim/metric divergence is intentional: the Church
 *   claims this is divine law (natural, inevitable, coordinative); the
 *   authored metrics describe substantially extractive, actively enforced
 *   suppression of autonomous kinship violence.
 *
 * KEY AGENTS:
 *   - Ecclesiastical Authority (Church): sets doctrine on divine law, administers penitential discipline, claims interpretive monopoly on legitimate violence
 *   - Royal Authority: consolidates territorial power by delegitimizing autonomous feud violence, aligns with Church to enforce suppression
 *   - Feud-Obligated Kinship Groups: bear the core constraint—caught between kinship obligation (identity-constituting) and spiritual/legal prohibition (mortal sin, capital crime)
 *   - Broader Peasant Populations and Non-Aligned Settlements: caught as collateral damage in both the feud cycle and enforcement machinery
 *   - Reformed Nobility: benefit by aligning with Church teaching and renouncing blood feud, gain legitimacy and jurisdictional advancement
 *   - Theological Dissenters: excluded from official framework but contest the reading—argue feud vengeance is scripturally justified under conditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.89).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.91).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Blood-Feud Prohibition via Divine Law and Ecclesiastical Authority").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal/religious/political").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, 'd340c248-c5e0-4b57-a940-57639f3883bb').
narrative_ontology:cs_kernel_codification('d340c248-c5e0-4b57-a940-57639f3883bb', fixed_text).
narrative_ontology:cs_authority_grounding('d340c248-c5e0-4b57-a940-57639f3883bb', lineage).
narrative_ontology:cs_interpretation_layer_present('d340c248-c5e0-4b57-a940-57639f3883bb').
narrative_ontology:cs_reading_relation('d340c248-c5e0-4b57-a940-57639f3883bb', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('d340c248-c5e0-4b57-a940-57639f3883bb', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_axiom('d340c248-c5e0-4b57-a940-57639f3883bb', foundational, divine_prohibition_on_vengeance).
narrative_ontology:cs_axiom_status(divine_prohibition_on_vengeance, holdable).
narrative_ontology:cs_axiom_grounding('d340c248-c5e0-4b57-a940-57639f3883bb', divine_prohibition_on_vengeance, deontological).
narrative_ontology:cs_axiom('d340c248-c5e0-4b57-a940-57639f3883bb', foundational, ecclesiastical_monopoly_on_legitimate_violence_interpretation).
narrative_ontology:cs_axiom_status(ecclesiastical_monopoly_on_legitimate_violence_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('d340c248-c5e0-4b57-a940-57639f3883bb', ecclesiastical_monopoly_on_legitimate_violence_interpretation, conventional).
narrative_ontology:cs_reference_frame('d340c248-c5e0-4b57-a940-57639f3883bb', divinely_ordered_justice_through_church_authority).
narrative_ontology:cs_drift_state('d340c248-c5e0-4b57-a940-57639f3883bb', late_medieval_period_300_years_after_initial_prohibition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d340c248-c5e0-4b57-a940-57639f3883bb', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, royal_authority).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, feud_obligated_kinship_groups).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, broader_peasant_populations).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, non_aligned_settlements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, reformed_nobility).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, excommunicated_feuders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Church (bishops, monastic orders, papal authority) teaches that blood vengeance is a violation of divine law—God prohibits killing, and only God and His appointed earthly representatives (Church and anointed king) hold legitimate authority to administer violence. The Church enforces this doctrine through the sacrament of confession (where priests interrogate penitents about feud participation), penitential discipline (public or private acts of contrition), and excommunication (exclusion from sacraments and Christian community) for those who continue blood feuding. The teaching expands ecclesiastical jurisdiction into the moral regulation of violence, a domain previously governed by kinship law. The Church benefits by monopolizing the interpretation of legitimate violence and by gaining leverage over nobility through the threat of excommunication.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_authority, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% The emerging or consolidating monarchy uses the Church's teaching to delegitimize autonomous feud violence and to consolidate the royal monopoly on legitimate force. The king frames blood-feud obligation as both sinful (violating divine law) and treasonous (violating royal justice). Royal courts prosecute feuders as criminals, and the monarchy benefits from reduced competitive violence that would fragment territory and challenge royal consolidation. The Church and monarchy collaborate in enforcement: bishops provide spiritual authority and surveillance (through confession), and royal courts provide coercive power (courts, punishments, execution). Royal authority benefits by leveraging ecclesiastical legitimacy to suppress feud violence that would otherwise compete with royal jurisdiction.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, royal_authority, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, royal_authority, agenda_setter).

% Kinship groups whose traditional law required them to prosecute blood vengeance when one of their members was harmed now face a paradox: their cultural and relational identity depends on maintaining kinship obligation (refusing to prosecute a blood-debt brings shame and loss of standing), but the Church and crown now define feud obligation as mortal sin (condemning the soul) and as treason (condemning the body to execution or mutilation). They are caught between two legal systems—kinship obligation and ecclesiastical/royal law—with no way to satisfy both. Their exit from feud obligation requires renouncing the kinship identity that constitutes their place in society, their claims to inheritance, their alliances, and their protection.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, feud_obligated_kinship_groups, payer,
    moderate, biographical, identity_locked, regional).

% Non-noble farming populations and village communities who have no formal participation in feud obligation but suffer collateral destruction from both the feud cycle (raids, destruction of crops, theft of animals, highway robbery by feuding parties) and from ecclesiastical/royal enforcement sweeps (soldiers hunting feuders may destroy non-combatant settlements, seize crops to support military operations, torture or execute suspected harborer of feuders, impose punitive taxes). They benefit marginally from reduced feud violence when enforcement is effective, but they experience the enforcement machinery as arbitrary violence. They cannot exit the enforcement zone and have no voice in either feud or pacification decisions.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, broader_peasant_populations, payer,
    powerless, biographical, trapped, regional).

% Communities (monasteries, towns, merchant enclaves) that do not belong to a kinship-group structure or that seek to remain neutral in feud disputes are victimized by both the feud cycle (raids for provisions, forced tribute, threats) and by enforcement machinery (suspicion of harboring feuders, punishment for non-cooperation with Church/royal authorities, destruction to deny resources to suspected feuders). Their neutrality is unstable: they can be forced to provide food, shelter, or intelligence by either feuding parties or by enforcement authorities. They bear the costs of both systems without control over either.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, non_aligned_settlements, payer,
    powerless, biographical, trapped, regional).

% Noble houses that voluntarily renounce blood-feud obligation and adopt the Church's teaching gain significant advantages: they are positioned as divinely favored, civilized, and aligned with ecclesiastical and royal authority; they receive preferential treatment in royal courts (appointments, land grants, favorable judgments); they gain competitive advantage over feud-obligated nobility that faces legal jeopardy and excommunication. Their renunciation of feud also signals loyalty to the emerging royal monopoly on legitimate force. They benefit by being early adopters of the new regime and by the suppression of their feud-rival nobility.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, reformed_nobility, beneficiary,
    powerful, generational, mobile, national).

% Members of kinship groups who have been caught feuding or who refuse to accept penitential discipline face excommunication: exclusion from sacraments (confession, communion, last rites), exclusion from Christian burial, social ostracism mediated through the Church. They also face criminal prosecution by royal courts (imprisonment, mutilation, execution, forfeiture of lands). The penitential system offers a theoretical exit pathway—public confession, acts of contrition, submission to ecclesiastical judgment—but accepting penitential discipline requires public renunciation of honor codes and acceptance of ecclesiastical authority over kinship law. Many excommunicated feuders spend years or decades under penitential discipline or in exile.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, excommunicated_feuders, payer,
    moderate, biographical, constrained, regional).

% Christian communities, theologians, and lay groups that contest the Church's reading of divine law—arguing that blood vengeance can be justified under certain scriptural precedents (divine commands to restore justice, defense of honor against violation, kinship obligation as divinely mandated in Old Testament law). They argue that the Church's prohibition exceeds ecclesiastical authority or that scripture permits blood vengeance under conditions of grave wrong. They are structurally excluded from the official ecclesiastical framework but their alternative readings feed continued feud practice and create friction in enforcement. Some are pursued as heretics; others operate in theological dissent networks outside formal Church channels.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, theological_dissenters, excluded,
    moderate, generational, trapped, regional).

% An external analytical position from which to assess the constraint as one reading of the feud-obligation kernel: how does the ecclesiastical reinterpretation (divine law prohibits vengeance, legitimate violence derives from God via Church/crown) construct a monopoly on legitimate violence by redefining the kernel practice? How does spiritual sanction (penitence, excommunication) function as enforcement alongside criminal law? How do the three readings (pacification, extraction-cycle, stateless-coordination) assign different victim sets and beneficiaries to the same kernel?
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single authoritative arbiter (the Church and anointed royal authority) for determining what violence is legitimate and under what conditions, replacing kinship-group adjudication of justice with centralized ecclesiastical/royal judgment. Solves the coordination problem of 'who decides whether violence is justified?' by asserting that only God and God's appointed representatives hold that authority. Redirects disputes over justice (which kinship law handles autonomously) into ecclesiastical channels (confession, penitential discipline) and royal courts.
% TRANSFER_FUNCTION: Transfers jurisdictional authority over justice from kinship groups to ecclesiastical and royal institutions. Transfers compliance labor from feud-obligated groups (who must accept penitential discipline and undergo spiritual interrogation) to the Church (which administers confession, penance, excommunication). Transfers legitimacy from kinship-based honor codes to divinely sanctioned institutional authority. Transfers spiritual authority from diverse Christian communities to the centralized Church hierarchy. Transfers coercive capacity from kinship enforcement to royal military and judicial apparatus.
% ABSENT_VOICES: Feud-obligated kinship groups are formally included but under severe coercion—their objections to the prohibition are treated as sinful resistance and are not given substantive hearing in ecclesiastical debate. Theological dissenters and alternative Christian readings of scripture are structurally excluded from the official framework—heresy charges suppress their voices. Broader peasant populations and non-aligned settlements have no formal voice in the definition of legitimate violence but suffer enforcement consequences. Women within feuding kinship groups are not explicitly represented in feud-obligation debates but are affected by enforcement (loss of male protectors to execution, loss of inheritance through criminalization, vulnerability to rape during enforcement sweeps). Indigenous or pre-Christian traditions that may have legitimized vengeance differently are excluded by the framing of the kernel itself as a Christian problem.
% DISAPPEARANCE_RATIONALE: If the constraint (blood-feud prohibition via divine law and ecclesiastical/royal authority) disappeared overnight, kinship groups would revert to autonomous adjudication of blood-debt and would resume blood-feud obligation as the primary justice mechanism in the absence of centralized authority. Ecclesiastical jurisdiction over moral violence questions would shrink dramatically, and the Church would lose a major tool for expanding its authority into secular law. Royal monopoly on legitimate force would face renewed competition from autonomous kin-group violence, fragmentation would increase, and territorial consolidation would stall or reverse. The institutional reorganization that the constraint enabled would collapse: reformed nobility would lose their competitive advantage; excommunication would lose its force as a sanction (no incentive to conform if feud obligation returns); peasant populations might face worse violence under returned feud dynamics, or might benefit from reduced enforcement sweeps. The constraint is foundational to the entire political reorganization of medieval Christian societies.
% FOUNDING_PROBLEM: Blood-feud obligations created destructive cycles of reciprocal violence that (1) prevented territorial consolidation and weakened kingdoms against external enemies; (2) enabled parallel justice systems (kinship adjudication competing with centralized authority); (3) fragmented political authority and made rule unenforceable; (4) created unpredictable violence that damaged commerce and settlement. The Church and emerging royal authority sought to solve this problem by monopolizing legitimate violence and delegitimizing autonomous feud obligation as violation of divine law.
% FOUNDING_PROBLEM_CORROBORATION: Royal chroniclers and ecclesiastical authorities attest the founding problem and attest its solution by the constraint. Kinship-group traditions and dissenting theological voices attest that the problem is artificially constructed—that blood vengeance was legitimate under their own law and theology and performed essential justice functions. Historical record (continuation of feud violence despite prohibition, persistence of secret feuding, endemic enforcement failure across centuries) suggests the problem persists unsolved and the 'solution' has merely displaced feud into covert channels. Independent observers from multiple jurisdictions note the gap between ecclesiastical claims of solving blood-feud violence and the persistence of feud activity in practice.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__christianized_pacification_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__christianized_pacification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__christianized_pacification_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.71 to 0.89 over the interval as ecclesiastical and royal enforcement machinery matures and hardens. Early enforcement (t=0) meets significant resistance from feud-obligated kinship groups still operating under traditional obligation; suppression is incomplete, theater ratio is lower (real enforcement and real resistance both present). By t=300, suppression has reached 0.91: ecclesiastical machinery (confession, penitence, excommunication) and royal courts now function as routine enforcement; kinship groups either comply, flee jurisdiction, or engage in covert feuding. Theater ratio rises to 0.62 because an increasing share of the enforcement apparatus maintains symbolic/ritual compliance (public renunciation of feud, penitential displays, court oaths) that does not fully end autonomous violence but creates the appearance of conformity. Extractiveness plateaus at 0.89 because the constraint cannot achieve total suppression—feud violence persists in forms obscured from enforcement machinery, but the constraint's core extraction (jurisdictional authority, compliance labor, legitimacy transfer) is near-complete. The shared time grid ensures every metric is authored at every point; measurements track both the enforcement ratchet (suppression rising) and the increasing dramatization of compliance (theater rising).
 *
 * PERSPECTIVAL GAP:
 *   The ecclesiastical/royal authority seats should compute this constraint as rope (genuine coordination with modest overhead). The feud-obligated kinship-group seats should compute it as snare (suppression of their autonomous authority, extraction of their compliance, spiritual coercion). The identity-locked exit option (for kinship groups) means they cannot arbitrage their way to beneficiary status—escape requires renouncing the relational identity that grounds them in their society. Reformed nobility sit near the beneficiary end: they reframe their exit from feud obligation as moral enlightenment and gain preferential status. Peasant populations sit deep in the victim end: they benefit from reduced feud violence but suffer increased enforcement sweeps and arbitrary punishment.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authority (institutional power, arbitrage exit options, continental scope) derives substantial d-beneficiary position: they set the doctrine, control penitential machinery, and expand their jurisdictional reach. Royal authority (institutional power, arbitrage, national scope) is secondary beneficiary: they consolidate monopoly on legitimate force and gain enforcement labor from the Church. Feud-obligated kinship groups (moderate power, identity-locked exit, regional scope) sit at extreme target position: exit from the constraint requires dissolving their kinship identity, which is impossible without losing all social standing and protection. Peasant populations and non-aligned settlements (powerless, trapped exit) sit at absolute-target positions: they benefit slightly from reduced feud raids but suffer from enforcement sweeps. Theological dissenters are excluded entirely from the framework, rendering them invisible in the official directionality calculus but present as sources of resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows signs of mandatrophy resolution: the founding problem (blood-feud cycles preventing consolidation) remains live in the formal doctrine but contested in practice. Historical record shows feud violence continued for centuries after the prohibition despite severe penalties. The constraint persists not because it solves the founding problem (the problem mutated into covert feuding) but because ecclesiastical and royal authorities benefit from the jurisdictional authority it grants them. Theater ratio rising to 0.62 indicates theatrical maintenance—public penitential ceremonies, court oaths renouncing feud, and symbolic compliance that does not fully arrest the underlying obligation. The mismatch between founding-problem status (contested) and disappearance verdict (world-rearranges) points to mandatrophy: if the constraint vanished, blood-feud obligations would resurface as the primary justice mechanism, but many populations have already internalized ecclesiastical teaching (generations raised under it), suggesting the constraint has partially ossified into normalized suppression. The theater ratio and suppression plateau suggest the constraint approaches piton territory—the founding problem is no longer solved, but ecclesiastical and royal interests in maintaining it remain strong enough to sustain performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the blood-feud obligation correctly read as a violation of divine law (the Church''s reading), or is the Church''s prohibition itself a contested reinterpretation that benefits ecclesiastical authority (the extraction-cycle and stateless-coordination readings)?',
    'Examination of theological dissent records, alternative scriptural interpretations, and historical evidence of whether the prohibition actually reduced feud violence or merely displaced it. Compare societies that adopted the prohibition against those that did not.',
    'If the ecclesiastical reading is the only tenable one, the constraint is a justified coordination mechanism. If alternative readings are defensible and systematically excluded, the constraint is more snare than rope—a cover story for authority extraction. This determines whether the constraint should be classified as tangled-rope (real coordination + extraction) or closer to pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the ecclesiastical interpretation is the only coherent reading of divine law or a contested reinterpretation serving institutional interests.').

omega_variable(
    identity_lock_durability,
    'Is the identity-lock that binds feud-obligated kinship groups to their obligation structural (relational identity constituted through the kinship framework) or internalized (participants have absorbed ecclesiastical teaching and now police themselves)?',
    'Post-prohibition transgenerational data: do descendants of feuding groups maintain feud obligations when enforcement weakens, or do they treat the ecclesiastical prohibition as internalized norm? How long does it take for escape from feud obligation to become normal rather than shameful?',
    'If identity-locked, the constraint''s suppression is external and will collapse if enforcement machinery fails. If internalized, suppression persists even after institutional enforcement erodes—the constraint becomes self-perpetuating and approximates a mountain (feels inevitable, natural). This determines whether the suppression metric should be interpreted as structural coercion or as successfully internalized normative teaching.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_durability, empirical, 'Whether feud-obligation exit requires only institutional permission or also identity transformation.').

omega_variable(
    enforcement_hidden_costs,
    'What is the full cost to ecclesiastical and royal authority of enforcing the blood-feud prohibition—in enforcement labor, military resources, judicial administration, and loss of feud-derived intelligence networks?',
    'Historical accounting of Church and royal resources devoted to anti-feud enforcement compared to resources derived from expanded ecclesiastical jurisdiction and consolidated royal authority.',
    'If enforcement costs exceed gains in authority, the constraint is less extractive than authored (0.89 may be too high). If gains substantially exceed costs, the constraint is even more extractive (hidden rents). This affects theater-ratio interpretation: high enforcement cost + modest gains = theater is necessary performance; low enforcement cost + high gains = theater is surplus performance masking pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_hidden_costs, empirical, 'Whether ecclesiastical and royal authority extract genuine net rents from enforcing the prohibition.').

omega_variable(
    alternative_reading_exclusion,
    'Are theological dissenters who contest the Church''s reading of divine law structurally foreclosed from the framework, or merely coexisting with it?',
    'Analysis of heresy trials, theological suppression, and whether alternative readings had institutional advocates or remained marginal. If the Church actively hunted down dissenters, that signals forclosure + suppression; if dissenters coexisted peaceably (or were tolerated in certain regions), the readings coexist.',
    'Forecloses = this reading rules out the stateless and extraction-cycle readings within any single institutional framework (high internal coherence, monolithic claim). Coexists = multiple readings remain live despite doctrinal claims to monopoly (lower internal coherence, contested from within). This determines whether the cs_structure.reading_relations should use forecloses or coexists_with for the dissident readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_exclusion, empirical, 'Whether the ecclesiastical reading achieves logical foreclosure of dissident theologies or merely institutional dominance.').

omega_variable(
    suppression_internalization_trajectory,
    'As the suppression requirement reaches near-total (0.91), is the plateau a sign of maximum enforcement capacity reached, or a sign of successful internalization where the constraint no longer requires external force?',
    'Time-series analysis of enforcement intensity vs. compliance patterns: if enforcement escalates while violation rates stay constant, the suppression plateau reflects capacity limits. If enforcement intensity plateaus while violation rates continue to decline, internalization is occurring.',
    'Capacity limit = the constraint remains extractive and will collapse if enforcement relaxes. Internalization = the constraint succeeds in reshaping populations'' norms and becomes self-sustaining (approaches mountain status). This affects whether the constraint is best understood as temporarily stable snare or successfully naturalized extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_trajectory, empirical, 'Whether near-total suppression is an equilibrium of enforcement effort or the beginning of norm internalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(feud_tr_t0, observed).
narrative_ontology:measurement(feud_tr_t50, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement_basis(feud_tr_t50, observed).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 100, 0.46).
narrative_ontology:measurement_basis(feud_tr_t100, observed).
narrative_ontology:measurement(feud_tr_t150, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 150, 0.52).
narrative_ontology:measurement_basis(feud_tr_t150, observed).
narrative_ontology:measurement(feud_tr_t200, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 200, 0.58).
narrative_ontology:measurement_basis(feud_tr_t200, observed).
narrative_ontology:measurement(feud_tr_t250, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 250, 0.61).
narrative_ontology:measurement_basis(feud_tr_t250, observed).
narrative_ontology:measurement(feud_tr_t300, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 300, 0.62).
narrative_ontology:measurement_basis(feud_tr_t300, observed).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(feud_be_t0, observed).
narrative_ontology:measurement(feud_be_t50, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 50, 0.76).
narrative_ontology:measurement_basis(feud_be_t50, observed).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 100, 0.81).
narrative_ontology:measurement_basis(feud_be_t100, observed).
narrative_ontology:measurement(feud_be_t150, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 150, 0.85).
narrative_ontology:measurement_basis(feud_be_t150, observed).
narrative_ontology:measurement(feud_be_t200, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 200, 0.88).
narrative_ontology:measurement_basis(feud_be_t200, observed).
narrative_ontology:measurement(feud_be_t250, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 250, 0.89).
narrative_ontology:measurement_basis(feud_be_t250, observed).
narrative_ontology:measurement(feud_be_t300, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 300, 0.89).
narrative_ontology:measurement_basis(feud_be_t300, observed).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(feud_su_t0, observed).
narrative_ontology:measurement(feud_su_t50, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(feud_su_t50, observed).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 100, 0.78).
narrative_ontology:measurement_basis(feud_su_t100, observed).
narrative_ontology:measurement(feud_su_t150, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 150, 0.83).
narrative_ontology:measurement_basis(feud_su_t150, observed).
narrative_ontology:measurement(feud_su_t200, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 200, 0.87).
narrative_ontology:measurement_basis(feud_su_t200, observed).
narrative_ontology:measurement(feud_su_t250, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 250, 0.9).
narrative_ontology:measurement_basis(feud_su_t250, observed).
narrative_ontology:measurement(feud_su_t300, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 300, 0.91).
narrative_ontology:measurement_basis(feud_su_t300, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__christianized_pacification_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__christianized_pacification_reading, 0.2).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__extraction_cycle_reading).

% DUAL FORMULATION NOTE:
% The feud-obligation kernel admits three structurally distinct constraint readings: (1) christianized_pacification_reading (THIS file) — feud obligation as violation of divine law, requiring ecclesiastical suppression via spiritual authority; (2) stateless_coordination_reading — feud obligation as self-enforcing deterrent mechanism necessary for justice in absence of centralized authority (makes sense only if prohibition is NOT assumed); (3) extraction_cycle_reading — feud obligation as resource drain preventing territorial consolidation, reinterpreted as dysfunctional cycle rather than coordinated justice. All three assess the same kernel practice (kinship-group blood vengeance) but instantiate different beneficiary/victim structures and different ε values. The readings do not coexist within a single institutional framework (though historically multiple readings were contested simultaneously across jurisdictions). This reading privileges the ecclesiastical reinterpretation and should be read in conjunction with the sibling readings to understand the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
