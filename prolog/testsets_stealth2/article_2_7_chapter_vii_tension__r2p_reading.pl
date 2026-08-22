% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__r2p_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: article_2_7_chapter_vii_tension__r2p_reading
 *   human_readable: Responsibility to Protect Reading of the Article 2(7)/Chapter VII Tension
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   The Charter kernel collides two texts: Article 2(7) guarantees that
 *   nothing authorizes intervention in matters essentially within domestic
 *   jurisdiction, while Chapter VII empowers the Council to act against
 *   threats to the peace. This file instantiates the r2p_reading of that
 *   kernel: sovereignty conditional on protecting populations, with
 *   systematic atrocity triggering an international responsibility to
 *   intervene. Under the fixed referent rule, epsilon is authored for the
 *   standing arrangement under contest — the
 *   conditional-sovereignty/intervention-responsibility regime itself — as
 *   this reading assesses it: high, because its operation strips targeted
 *   authorities of territorial control, kills civilians inside intervened
 *   states, and erodes the non-intervention assurance held by every state,
 *   even where the reading judges much of that stripping justified at
 *   threshold. Endorsement of a threshold is not denial of the extraction's
 *   magnitude. The sibling reading authors its own story with its own
 *   epsilon; the two are linked in network.affects_constraints. KEY AGENTS
 *   (by structural relationship): - security_council_permanent_five: Agenda
 *   setter (institutional/arbitrage) — veto gate over activation; converts
 *   the responsibility into discretionary licensing - persecuted_populations:
 *   Primary intended beneficiary (powerless/trapped) — receives protection
 *   when the frame activates, abandonment when it does not -
 *   intervening_great_powers: Secondary beneficiary (powerful/arbitrage) —
 *   collects legitimated force projection, elects its engagements -
 *   targeted_state_authorities: Primary target (powerful/trapped) — loses
 *   territorial control and sovereign shield; cessation does not restore it -
 *   civilian_populations_in_intervened_states: Collateral bearer and excluded
 *   voice (powerless/trapped) — bears intervention costs, decides nothing -
 *   state_sovereignty_norm: Eroded non-agent (institutional/trapped) — the
 *   non-intervention assurance, listed for completeness, excluded from
 *   derivation - general_assembly_majority_states: Excluded voice
 *   (organized/constrained) — objects to selectivity with no lever -
 *   humanitarian_advocacy_network: Identity-locked beneficiary
 *   (moderate/identity_locked) — collects mission meaning; exit dissolves
 *   founding purpose - r2p_doctrine_scholars: Analytical observer
 *   (analytical/analytical)
 *
 * KEY AGENTS:
 *   - security_council_permanent_five: agenda setter, institutional power, arbitrage exit — veto gatekeeper converting universal responsibility into discretionary license
 *   - persecuted_populations: primary intended beneficiary, powerless, trapped — protection arrives or does not, entirely by external decision
 *   - intervening_great_powers: secondary beneficiary, powerful, arbitrage — legitimated force projection, engagement fully elective
 *   - targeted_state_authorities: primary target, powerful, trapped — sovereign control stripped once framed; Libya showed cessation buys nothing
 *   - civilian_populations_in_intervened_states: collateral payer with excluded secondary role, powerless, trapped — bears strikes and aftermath, consulted never
 *   - state_sovereignty_norm: non-agent victim of erosion, recorded for completeness, excluded from directionality and gain derivation
 *   - general_assembly_majority_states: excluded voice, organized, constrained — annual objection with zero outcome leverage
 *   - humanitarian_advocacy_network: identity-locked beneficiary, moderate — mission fused to the doctrine, exit equals dissolution
 *   - r2p_doctrine_scholars: analytical observer — maps commitments and drift, no stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, 0.72).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__r2p_reading, 0.5).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__r2p_reading, "Responsibility to Protect Reading of the Article 2(7)/Chapter VII Tension").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__r2p_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__r2p_reading, '8a44b3f3-cff1-4834-86d5-531c3c53b1eb').
narrative_ontology:cs_kernel_codification('8a44b3f3-cff1-4834-86d5-531c3c53b1eb', fixed_text).
narrative_ontology:cs_authority_grounding('8a44b3f3-cff1-4834-86d5-531c3c53b1eb', lineage).
narrative_ontology:cs_interpretation_layer_present('8a44b3f3-cff1-4834-86d5-531c3c53b1eb').
narrative_ontology:cs_reading_relation('8a44b3f3-cff1-4834-86d5-531c3c53b1eb', article_2_7_chapter_vii_tension__sovereignty_first_reading, forecloses).
narrative_ontology:cs_axiom('8a44b3f3-cff1-4834-86d5-531c3c53b1eb', foundational, sovereignty_conditioned_on_population_protection).
narrative_ontology:cs_axiom_status(sovereignty_conditioned_on_population_protection, holdable).
narrative_ontology:cs_axiom_grounding('8a44b3f3-cff1-4834-86d5-531c3c53b1eb', sovereignty_conditioned_on_population_protection, deontological).
narrative_ontology:cs_axiom('8a44b3f3-cff1-4834-86d5-531c3c53b1eb', secondary, chapter_vii_extends_to_mass_atrocity_threats).
narrative_ontology:cs_axiom_status(chapter_vii_extends_to_mass_atrocity_threats, holdable).
narrative_ontology:cs_axiom_grounding('8a44b3f3-cff1-4834-86d5-531c3c53b1eb', chapter_vii_extends_to_mass_atrocity_threats, empirically_contingent).
narrative_ontology:cs_reference_frame('8a44b3f3-cff1-4834-86d5-531c3c53b1eb', conditional_sovereignty_common_interest).
narrative_ontology:cs_drift_state('8a44b3f3-cff1-4834-86d5-531c3c53b1eb', post_libya_backlash_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8a44b3f3-cff1-4834-86d5-531c3c53b1eb', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, intervening_great_powers).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, security_council_permanent_five).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, humanitarian_advocacy_network).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, targeted_state_authorities).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, civilian_populations_in_intervened_states).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, state_sovereignty_norm).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__r2p_reading, conditional_sovereignty_principle).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__r2p_reading, protection_responsibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five states holding veto power over any Chapter VII action. They decide when the protection responsibility activates and when it does not: resolutions 1674 and 1894 framed the doctrine, resolution 1973 executed it in Libya, and repeated vetoes buried it for Syria. Each member retains complete freedom to block application to clients and allies while the doctrine binds no one else to act. Converts a universal-sounding responsibility into discretionary licensing held in five hands.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, security_council_permanent_five, agenda_setter,
    institutional, generational, arbitrage, global).

% Ethnic, religious, and political groups facing systematic violence — Darfuris from 2003, residents of Benghazi in 2011, Rohingya after 2017. They cannot protect themselves against their own state and their only exit is flight. When the frame activates they receive no-fly zones, sanctions relief valves, mediation, or military protection; when a veto blocks it (Syria) they receive nothing and the vocabulary of protection becomes a taunt.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations, beneficiary,
    powerless, biographical, trapped, national).

% Coalition states, principally NATO members, that execute authorized protection operations. They gain legitimated force projection, operational positioning, reputational capital as protectors, and precedent value for future discretionary action. Participation is elective: most declined Darfur and Syria entirely, choosing engagements by interest while citing the same doctrine.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, intervening_great_powers, beneficiary,
    powerful, biographical, arbitrage, continental).

% Governments whose atrocity commission or imminent risk activates the override — Qaddafi's regime in 2011 is the executed case. Once framed, appeals to Article 2(7) fail, assets freeze, commands are struck, and the mandate migrates from protection to removal: halting the atrocities did not halt the intervention in Libya. Their exit is ceasing the killing, yet cessation left them exposed anyway, so the trap closes from both sides.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, targeted_state_authorities, payer,
    powerful, biographical, trapped, national).

% Ordinary residents of targeted states regardless of persecution status. They bear airstrikes, no-fly-zone disruption, militia proliferation, state collapse, and the decade-long aftermath visible in post-2011 Libya. They decide nothing: authorization happens in New York and allied capitals, and no seat records their consent or refusal.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, civilian_populations_in_intervened_states, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__r2p_reading, civilian_populations_in_intervened_states, excluded).

% Non-agent entity recorded for narrative completeness: the Westphalian non-intervention assurance codified in Article 2(7), the one formal equalizer held identically by large and small states alike. Generalized conditionality thins it for everyone — every government's shield against armed foreign override weakens each time the exception is exercised or claimed. Because it is a norm and not an actor, it is excluded from directionality and gain derivation; its erosion is tracked narratively and through the omega layer.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, state_sovereignty_norm, payer,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(article_2_7_chapter_vii_tension__r2p_reading, state_sovereignty_norm).

% The G77 and Non-Aligned majority. They joined the 2005 consensus, attend the annual General Assembly dialogue on the doctrine every year since 2009, and produced the Brazilian responsibility-while-protecting proposal after Libya — and hold no enforcement lever over any of it. Their objections to selectivity generate reports, not outcomes. Exiting the UN is not a live option, so participation is mandatory and consequence-free.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, general_assembly_majority_states, excluded,
    organized, generational, constrained, global).

% NGOs, research centres, UN focal-point networks, and campaign arms whose organizational identity fused with the doctrine — the institutionalized never-again commitment. They collect mission relevance, funding streams, and moral standing from the framework's existence and lobby for activation everywhere. Selectivity contradicts their stated universalism, but exiting would mean dissolving their founding purpose, so they defend the frame even while documenting its failures.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, humanitarian_advocacy_network, beneficiary,
    moderate, biographical, identity_locked, global).

% International lawyers and political theorists who map the reading's commitments, trace the Libya precedent, audit selectivity, and publish assessments of whether conditional sovereignty is consolidating or collapsing. No material stake in activation or blocking; their exit is perfect and costless, which is what makes the seat observational.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, r2p_doctrine_scholars, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__r2p_reading, security_council_permanent_five).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__r2p_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the atrocity-response collective-action problem: absent a shared threshold and authorization path, responses collapse into either unilateral adventurism or paralytic deference to the domestic-jurisdiction bar. The doctrine pools response capacity, fixes a common trigger vocabulary, and assigns a single decision site.
% TRANSFER_FUNCTION: Moves life-and-death jurisdiction — the authority to protect, police, and govern a territory's population — from the targeted state's authorities to the Council gatekeepers and whichever coalition executes authorization; secondarily it thins the border-inviolability assurance held by every state, transferring existential security margin from all states to the gatekeeping five.
% ABSENT_VOICES: Populations of targeted states are decided-for in New York and allied capitals; no seat represents their consent or refusal — resolution 1973 heard the Arab League, not Libyans. Sovereignty-first adherents sit physically in the room, in Assembly debates and Council chambers, but hold no outcome-shaping lever, so their presence is formal rather than effective.
% DISAPPEARANCE_RATIONALE: If the conditional-sovereignty frame vanished overnight, intervention authority would revert to ad hoc unilateral humanitarian claims on the Kosovo pattern or to outright paralysis; the Council would lose its legitimation vocabulary for collective action; persecuted groups would lose their mobilization frame and their one argument against abandonment; targeted regimes would regain a complete shield; and the non-intervention assurance would snap back toward its pre-2001 thickness. Arrangements demonstrably depend on it.
% FOUNDING_PROBLEM: Reconcile the non-intervention rule with the demonstrated catastrophe of hiding atrocity behind it: Rwanda 1994 and Srebrenica 1995 showed sovereignty invoked as a shield for extermination, and the 2001 ICISS report with the 2005 World Summit built conditional sovereignty so protection duties could override the shield at defined thresholds.
% FOUNDING_PROBLEM_CORROBORATION: Russian, Chinese, and G77 delegations concede the Rwanda and Srebrenica failures in Assembly debate even as they reject expanded override — attestation from outside the beneficiary set. The ICISS commission's cross-regional membership and the African Union's Ezulwini Consensus attest independently; no party denies the founding problem, only this reading's answer to it.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__r2p_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__r2p_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__r2p_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__r2p_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__r2p_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claim is tangled_rope from structure: a genuine coordination function (pooled atrocity response with a fixed trigger and decision site) operating through asymmetric extraction (five-veto selectivity, elective intervention, mission creep past protection into regime change, diffuse sovereignty erosion) sustained by active enforcement (Council resolutions, no-fly zones, sanctions, asset freezes).
 *   
 *   Metrics describe operation. Extractiveness 0.72: the executed and threatened cases strip rulers of territory, kill bystander civilians, and tax every state's security assurance simultaneously. Suppression 0.50, raw and unscaled per the framework rule: non-consensuality is definitional, and once framed a state's Article 2(7) defense collapses, though intervener-side alternatives (Kosovo-style unilateralism) survive, which caps suppression below enforcement-monopoly levels. Accessibility_collapse 0.62 reflects that same asymmetry: total for framed targets, partial for potential executors. Resistance 0.65 is real and organized: the Brazilian responsibility-while-protecting initiative, the Small Five accountability pushes, sustained G77 and BRICS pushback, post-Libya veto hardening. Theater_ratio 0.50: genuine protective capacity (Kenya 2008's mediated settlement averted mass violence; the Benghazi evacuation was real) beside a thickening ritual layer — annual Assembly dialogues since 2009 that substitute discourse for capacity, classic Goodhart proxy drift.
 *   
 *   Temporal series share the single grid {0,4,8,12,16,20,24}, mapping 2001-2025. The suppression_requirement series is authored deliberately because enforcement-capacity change is the dynamic this story traces: machinery built through the 2005 adoption and the 2006-2011 resolution sequence, peaked around the Libya execution, then decayed operationally after the backlash while rhetorical maintenance continued — a built-up hump with a falling tail, not a flat picture, which is exactly why a series rather than the scalar alone is warranted.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently and should. From the P5 seat the arrangement is a legitimate collective-security instrument they alone can calibrate, and its selectivity is prudence. From targeted_state_authorities it is selective dispossession, applied against rivals and spared for patrons, with no exit even at compliance. From persecuted_populations it is the difference between abandonment and survival — the same structure reads as lifeline. From the sovereignty-first constituency it is the thinning of their last formal equalizer. Same-level dynamics sharpen the gap: the five permanent members and the ten elected Council members occupy the same chamber at nominally identical standing, yet the permanent five hold arbitrage (block anything, bind no one) while the elected ten are constrained spectators, and the Assembly majority is further down still. Inter-institutionally, regional bodies experience it differently again: the African Union claims subsidiary responsibility it cannot exercise without Council leave. None of these divergences is adjudicated by the authored claim; the engine computes them from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: persecuted_populations (trapped, subsidized with survival), intervening_great_powers (arbitrage, the strongest dampener toward subsidy), security_council_permanent_five (arbitrage atop the gate), humanitarian_advocacy_network (identity_locked, collecting mission-meaning). Victim declarations drive high directionality: targeted_state_authorities and civilian_populations_in_intervened_states are trapped, so their effective extraction amplifies rather than damps. state_sovereignty_norm sits in the victims array but carries agent:false, so it is excluded from directionality and gain computation by design; its erosion is recorded narratively and through the omega layer rather than fed into chi as though a doctrine collected or paid. general_assembly_majority_states receives no directional declaration: per the R3 ruling, an authored absence must not steer classification, so the excluded seat stays commentary-grade.
 *
 * MANDATROPHY ANALYSIS:
 *   Authoring this as tangled_rope blocks two symmetrical mislabels. A pure-rope verdict would credit the coordination function while ignoring the veto-gated extraction — the selective license, the Libya slide from protection to overthrow, the assurance erosion taxing all states for the discretion of five. A pure-snare verdict would reduce everything to imperial cover while discarding the protections actually delivered — Kenya 2008 averted mass violence through the framework's diplomatic arm, and Benghazi 2011 was a genuine imminent massacre interrupted. Mandatrophy: the founding problem remains live (atrocities continue; the reconciliation problem recurs with every crisis), so no resolved-mandatrophy declaration is authored. The lifecycle risk this story tracks instead is Goodhart drift — theater_ratio climbing as annual debate substitutes for operational capacity — which the temporal series exposes without forcing a premature terminal verdict; if the trend continued toward a doctrine that is all dialogue and no capacity, the piton signature would become the honest successor classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position_r2p,
    'This constraint instantiates the r2p_reading of the kernel article_2_7_chapter_vii_tension; what structurally changes under the sibling sovereignty_first_reading?',
    'Compile the sibling story and compare: under sovereignty-first, the intervention license itself becomes the extraction object, the sovereignty assurance becomes a protected good rather than an eroded victim, and the beneficiary/victim sets invert around the same Charter text.',
    'Classification referents flip wholesale: epsilon reattaches to a different arrangement, the P5 gate reads as guarantor rather than capturer, and per-seat classifications invert. Cross-reading comparison is only valid story-to-story, never by averaging.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position_r2p, conceptual, 'Committer structure: one reading of a contested kernel, with a live sibling that would invert the structural data.').

omega_variable(
    veto_gatekeeping_extraction_share,
    'How much of the measured extraction is attributable to the five-member veto gate versus the conditional-sovereignty principle itself?',
    'Counterfactual governance comparison: observe extraction profiles under alternative decision sites — Assembly supermajority override on the Uniting for Peace pattern, African Union subsidiarity with guaranteed resourcing — and test whether selectivity and mission creep persist when the gate changes hands.',
    'If extraction is gatekeeping-driven, the principle could classify nearer rope under reformed governance and the fix is institutional redesign; if principle-driven, extraction is constitutive of conditional sovereignty as such and no gate relocation removes it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_gatekeeping_extraction_share, conceptual, 'Separating gatekeeper rent from doctrinal extraction.').

omega_variable(
    protection_to_regime_change_slide,
    'Does authorization-for-protection structurally slide into overthrow, as in Libya 2011, or was that migration a contingent choice of the executing coalition?',
    'Comparative mandate tracing across executed and declined cases — Kenya 2008 diplomacy, Cote d''Ivoire 2011, Libya 2011, the declined Darfur and Syria activations — coding whether mandates broadened, held, or were refused, and under what coalition configurations.',
    'If the slide is structural, extraction is baked into the mechanism and snare-drift signatures become the monitoring priority; if contingent, tightening mandate discipline could cap extraction without abandoning the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_to_regime_change_slide, empirical, 'Whether the Libya precedent reveals a constitutive or contingent failure mode.').

omega_variable(
    atrocity_deterrence_effect,
    'Does credible activation probability actually reduce atrocity onset or severity, or does the doctrine deter nothing while costing sovereignty assurance across all states?',
    'Comparative atrocity-incidence studies conditioning onset and escalation on doctrinal salience and activation credibility across the 2001-2025 window, controlling for intervention capability.',
    'A null deterrent effect would hollow the coordination function and accelerate theater/piton drift — the arrangement persists as performance; a robust effect would mean part of the measured extraction is the functional price of protection and strengthens the tangled_rope reading against snare reinterpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(atrocity_deterrence_effect, empirical, 'Empirical foundation of the claimed coordination function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__r2p_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(r2p_reading_tr_t0, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(r2p_reading_tr_t4, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(r2p_reading_tr_t8, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(r2p_reading_tr_t12, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(r2p_reading_tr_t16, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(r2p_reading_tr_t20, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement(r2p_reading_tr_t24, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 24, 0.5).

% Extraction over time
narrative_ontology:measurement(r2p_reading_be_t0, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(r2p_reading_be_t4, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(r2p_reading_be_t8, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(r2p_reading_be_t12, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(r2p_reading_be_t16, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(r2p_reading_be_t20, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(r2p_reading_be_t24, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 24, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(r2p_reading_su_t0, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(r2p_reading_su_t4, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(r2p_reading_su_t8, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(r2p_reading_su_t12, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(r2p_reading_su_t16, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(r2p_reading_su_t20, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(r2p_reading_su_t24, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 24, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__r2p_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension__sovereignty_first_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Article 2(7)/Chapter VII tension' covers two structurally distinct claims about what sovereignty is and when force may cross borders. Per the epsilon-invariance principle these are two stories, not one: this file (r2p_reading) authors epsilon 0.72 for the conditional-sovereignty/intervention-responsibility arrangement as the protection reading assesses it; the sibling (sovereignty_first_reading) authors its own epsilon for the sovereignty-assurance arrangement as the foundational reading assesses it. The upstream reading is the historically prior baseline (Charter-era orthodoxy through the Cold War); this reading creates downstream structural pressure on it — every exercised activation erodes the baseline's assurance — without resolving which reading governs. Both files link each other in affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
