% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Responsibility-to-Protect Reading of the Charter Sovereignty-Intervention Tension
 *   domain: international law/political philosophy/security studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   the fixed UN Charter text holding Article 2(7) non-intervention in
 *   tension with Chapter VII coercive authority; this file generates the
 *   r2p_reading, under which sovereignty is conditional on protecting
 *   populations and systematic atrocity triggers an international
 *   responsibility to act. The sibling reading (sovereignty_first_reading:
 *   consent or inter-state-aggression-only intervention) is a separate
 *   constraint story with its own epsilon, victims, and classification;
 *   nothing about it is averaged into this file. Epsilon's referent here is
 *   the standing arrangement under contest as it actually operates - a
 *   protective-responsibility regime filtered through Security Council veto
 *   discretion and applied selectively since the 2011 Libya conversion -
 *   assessed by this reading's own lights: a reading that holds protection to
 *   be owed reliably and impartially prices the selective, episodic,
 *   instrumentally steered delivery as a wide gap between obligation and
 *   operation, and that gap is carried by targeted states and by the
 *   shield-dependent states whose non-intervention guarantee the exceptions
 *   thin. The claimed type and the metrics below are independent authored
 *   facts: I claim tangled_rope as the structure I believe true (genuine
 *   protective coordination fused with veto-filtered asymmetric extraction)
 *   and author the metrics as descriptive of actual operation, without tuning
 *   either toward the other or toward any predicted engine output. KEY AGENTS
 *   (by structural relationship): - persecuted_civilian_populations: intended
 *   beneficiary (powerless/trapped) - receives episodic protection, bears the
 *   risk of abandonment - permanent_five_members: agenda-setter and
 *   discretionary beneficiary (institutional/arbitrage) - controls every
 *   activation - western_military_coalitions: operating beneficiary
 *   (institutional/mobile) - converts mandates into strategic latitude -
 *   targeted_territorial_states: primary payer (moderate/trapped) - loses
 *   autonomy and sometimes regime survival -
 *   sovereignty_reliant_small_states: diffuse payer (powerless/constrained) -
 *   non-intervention guarantee erodes - regional_security_organizations:
 *   dual-positioned operator (organized/constrained) - executes most real
 *   operations, bears their costs - r2p_advocacy_network: discourse
 *   beneficiary (organized/identity_locked) - careers fused to the doctrine's
 *   persistence - un_general_assembly_majority: excluded voice
 *   (organized/constrained) - adopted the limiting formula, is bypassed in
 *   practice - international_jurists: analytical observer (analytical) - maps
 *   the doctrine against text and practice
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, 0.72).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__r2p_reading, 0.58).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__r2p_reading, "Responsibility-to-Protect Reading of the Charter Sovereignty-Intervention Tension").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__r2p_reading, "international law/political philosophy/security studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__r2p_reading, '68b26548-776b-4a44-a010-fdcfae838856').
narrative_ontology:cs_kernel_codification('68b26548-776b-4a44-a010-fdcfae838856', fixed_text).
narrative_ontology:cs_authority_grounding('68b26548-776b-4a44-a010-fdcfae838856', extraction).
narrative_ontology:cs_interpretation_layer_present('68b26548-776b-4a44-a010-fdcfae838856').
narrative_ontology:cs_reading_relation('68b26548-776b-4a44-a010-fdcfae838856', article_2_7_chapter_vii_tension__sovereignty_first_reading, coexists_with).
narrative_ontology:cs_axiom('68b26548-776b-4a44-a010-fdcfae838856', foundational, sovereignty_conditional_on_population_protection).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_population_protection, holdable).
narrative_ontology:cs_axiom_grounding('68b26548-776b-4a44-a010-fdcfae838856', sovereignty_conditional_on_population_protection, deontological).
narrative_ontology:cs_axiom('68b26548-776b-4a44-a010-fdcfae838856', foundational, systematic_atrocity_triggers_collective_duty_to_act).
narrative_ontology:cs_axiom_status(systematic_atrocity_triggers_collective_duty_to_act, holdable).
narrative_ontology:cs_axiom_grounding('68b26548-776b-4a44-a010-fdcfae838856', systematic_atrocity_triggers_collective_duty_to_act, instrumental).
narrative_ontology:cs_reference_frame('68b26548-776b-4a44-a010-fdcfae838856', sovereignty_as_protective_stewardship).
narrative_ontology:cs_drift_state('68b26548-776b-4a44-a010-fdcfae838856', post_libya_backlash_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('68b26548-776b-4a44-a010-fdcfae838856', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, persecuted_civilian_populations).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, western_military_coalitions).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, r2p_advocacy_network).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, targeted_territorial_states).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_reliant_small_states).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, westphalian_sovereignty_norm).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, permanent_five_members).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, regional_security_organizations).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, regional_security_organizations).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__r2p_reading, conditional_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__r2p_reading, human_security_paradigm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live inside states whose governments or armed groups target them for identity. When the international arrangement activates for them they receive protection operations, safe corridors, or diplomatic pressure on their attackers; when it does not activate they are left with flight, local hiding, or submission. They cannot relocate out of the atrocity zone in most cases and have no vote in the authorization decisions taken on their behalf.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, persecuted_civilian_populations, beneficiary,
    powerless, immediate, trapped, national).

% Hold veto power over Security Council authorization, so every activation of protective intervention passes through their assent or abstention. Three of the five periodically lead or join protection operations; two routinely withhold authorization where their clients or interests are exposed. They draft the resolutions, set the invocation pace, and absorb little comparable exposure to the arrangement's downside.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, permanent_five_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__r2p_reading, permanent_five_members, beneficiary).

% Form ad hoc around capable militaries, principally NATO members, and execute air campaigns, no-fly zones, and stabilization deployments when authorization or great-power tolerance permits. Protective mandates recast strategically chosen operations in humanitarian language, and the coalitions decide which crises to join. They carry crew risk, expenditure, and post-operation governance entanglement.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, western_military_coalitions, beneficiary,
    institutional, biographical, mobile, global).

% Govern territories flagged for systematic atrocity. Once invoked, they face authorized coercion ranging up to regime-change operations, plus asset freezes, indictments, and diplomatic isolation, and they cannot conventionally resist a capable coalition. Leadership loses the working assumption that internal conduct stays internal; patrons and nuclear or alliance buffers soften but do not restore the prior shield.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, targeted_territorial_states, payer,
    moderate, biographical, trapped, national).

% Commit no atrocities and project no expeditionary force, but rely on the non-intervention default as their primary security guarantee against stronger neighbors and distant powers. Each widening of the protective exception thins that guarantee for them specifically, because they can neither deter an intervening coalition nor buy equivalent protection. Their counterweight is bloc voting in the General Assembly and treaty forums.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_reliant_small_states, payer,
    powerless, generational, constrained, global).

% Regional bodies such as ECOWAS and the African Union execute most of the actual protection operations on record, carrying troop casualties, financing shortfalls, and friction with member neighbors, in exchange for mandate legitimacy, partner training and equipment flows, and standing as recognized first responders. They cannot refuse every engagement without losing donor support, and they do not select targets the way global coalitions do.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, regional_security_organizations, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__r2p_reading, regional_security_organizations, payer).

% UN special-adviser office staff, dedicated research centres, and allied NGOs sustain the annual General Assembly dialogues, produce atrocity-risk assessment frameworks, and lobby capitals to appoint national focal points. Careers, funding lines, and institutional relevance depend on the doctrine staying on the agenda; walking away would dissolve the professional standing built around it.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, r2p_advocacy_network, beneficiary,
    organized, generational, identity_locked, global).

% The broad membership that adopted the narrow 2005 formulation precisely to confine protective action to Security Council authorization case by case. Their deliberations produce consensus texts that coalitions then bypass in practice; they retain votes and speech but not decision power over when the arrangement activates.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, un_general_assembly_majority, excluded,
    organized, generational, constrained, global).

% Academic and bench lawyers who map the doctrine against Charter text, International Court of Justice jurisprudence, and accumulated state practice. They publish the assessments both camps cite, collect no operational rents, and can move intellectually between readings of the Charter at no personal cost.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, international_jurists, observer,
    analytical, generational, analytical, global).

% The Charter's Article 2(7) non-intervention principle treated as a standing legal commitment. Each protective invocation writes another qualification into its practical scope: Chapter VII practice against internal catastrophes, protection-of-civilians mandates, and coalition operations mounted without authorization. The norm survives intact in text and rhetoric while its unconditional application recedes episode by episode. Listed for completeness; not an actor.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, westphalian_sovereignty_norm, payer,
    institutional, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(article_2_7_chapter_vii_tension__r2p_reading, westphalian_sovereignty_norm).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__r2p_reading, permanent_five_members).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__r2p_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Addresses a real collective-action failure: no single state has the incentive or standing capacity to halt mass atrocity inside another state, and unilateral intervention lacks legitimacy. The arrangement supplies a shared recognition threshold for when domestic repression becomes an international concern, pools military and humanitarian capacity, and converts would-be unilateral operations into multilaterally framed protection efforts.
% TRANSFER_FUNCTION: Moves decision authority over a territorial state's internal conduct from that state to the Security Council and ad hoc coalitions; moves legitimacy from the intervening power's own assertion to a protective frame; moves protection, episodically, to persecuted populations; and moves operational costs onto intervening militaries and regional organizations. Net direction of the transfer runs from targeted and shield-dependent states toward veto-holding discretion and coalition freedom of action.
% ABSENT_VOICES: Populations inside targeted states who oppose foreign military action are spoken for rather than consulted, by their own governments and by coalitions claiming protective mandates alike. The General Assembly majority that adopted the restrictive 2005 formulation sits formally in the room but is decision-inoperative whenever coalitions move without or around the Council. Sovereignty-first jurists exist as published dissent but hold no seat in authorization practice.
% DISAPPEARANCE_RATIONALE: Protection-of-civilians language anchors dozens of standing peacekeeping mandates; atrocity-prevention offices, focal-point networks, and the annual dialogue cycle exist only because the arrangement does. Overnight removal would strand that architecture, remove the episodic recourse persecuted populations currently hold (Kenya 2008, Gambia 2017 style diplomatic-and-force responses), and force great powers back to open interest claims for interventions they would still conduct. The rearrangement is partial: power-driven interventions predate the doctrine and would survive it, which is why the sovereignty-first camp disputes that anything essential depends on it.
% FOUNDING_PROBLEM: Mass atrocity under sovereign cover with no collective response: Rwanda 1994 and Srebrenica 1995 exposed an order in which the legal shield of non-intervention guaranteed that genocide could proceed unopposed. The arrangement was built to make protection of populations a standing international responsibility rather than an ad hoc charity.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the General Assembly majority that adopted the 2005 paragraphs included overwhelmingly non-beneficiary states acting on documented failure, UN Commission of Inquiry reporting (Darfur onward) continuously attests that the founding problem recurs, and survivor testimony archives together with non-advocate international-law scholarship confirm both the problem's reality and its unresolved status. Target-state governments and the sovereignty-first camp dispute the status, which is recorded rather than resolved here.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__r2p_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__r2p_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__r2p_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   Claim/metric independence: tangled_rope is asserted because both halves are structurally present - the coordination achievement (shared atrocity threshold, pooled capacity, legitimated response) and the extraction (activation gated by five vetoes, delivery steered by intervener interest) - while the metrics describe operation as found. Extractiveness 0.72 reflects the size of the obligation-delivery gap charged against targeted and shield-dependent states. Suppression 0.58 is a raw structural property, unscaled by power or scope in this framework: a flagged state faces coalition coercion it cannot conventionally resist, and non-P5 states conduct internal affairs under a standing chill, though the constraint's suppressive force is episodic rather than routinized. Theater_ratio 0.60: the functional residue (mandate language, occasional diplomatic-and-force saves) is real but dwarfed by two decades of dialogues, reports, focal-point appointments, and anniversary statements unaccompanied by new protective delivery since roughly 2017. Accessibility_collapse 0.55: alternatives collapse nearly completely for the persecuted (flight or hiding), only partially for states, which can still purchase deterrence or patronage. Resistance 0.62: organized and sustained - the post-Libya counter-mobilization (Brazil's responsibility-while-protecting initiative, sovereignty-bloc deadlock over Syria, small-state insistence on the 2005 limits). Temporal data run on ONE shared grid {0,5,10,15,20,25}; every tracked metric carries an authored value at every point. The suppression_requirement series deliberately tracks enforcement-capacity change rather than structural suppression: enforcement effort built through the Libya peak (authorization politics demanded active defense of the doctrine's use) and then decayed as invocation froze and maintenance shifted to ritual dialogue - hence the falling tail, which is enforcement decay, not relaxation of the load on targets, which the static scalar carries.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural data. The permanent-five seat experiences the arrangement as managed discretion - a scarce authorization good it allocates - and should classify near the beneficiary end despite paying credibility costs. Targeted territorial states, trapped once invoked, experience a legitimacy trap: the same text that promised non-intervention becomes the instrument of their coercion. Persecuted populations hold the highest stakes and least power: a lifeline that arrives irregularly, whose anticipation can also mark them as fifth columns. Shield-reliant small states experience slow norm erosion they did not cause and cannot reverse individually - their remedy is coalition voting (General Assembly majorities, Uniting-for-Peace resolutions), a coalition path historically blunted by great-power procedural control. Regional operators sit between: mandated legitimacy flowing in, casualties and unfunded mandates flowing out. The engine derives these divergent classifications from the declared positions; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: persecuted populations are subsidized when the arrangement fires (directionality near the full-beneficiary end, amplified by trapped exit - they cannot arbitrage away their dependence), coalitions receive mandate legitimacy that lowers the political price of chosen operations, and the advocacy network collects agenda relevance. Victim declarations mirror the cost side: targeted states bear the sharpest extraction (high directionality, trapped - no exit from the invoked label), and small states bear diffuse erosion (high directionality, constrained - patronage and alliance purchases are partial exits). The permanent five are declared agenda_setter with secondary beneficiary standing rather than pure beneficiaries because their position is dual: they collect allocation discretion while absorbing credibility and operational costs, which places them nearer symmetric than the derivation from beneficiary-status alone would suggest; the dual-role declaration encodes this without an override. The sovereignty norm is entered with agent:false precisely so the story can honor the delta's claim that the norm itself is injured without letting a non-actor feed the directionality arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - atrocity under sovereign cover meeting no collective response - is live (Syria, Ethiopia, Myanmar, Sudan recur on schedule), so no mandatrophy resolution is declared and none is due. The tangled_rope classification prevents two symmetrical errors. Mislabeling the arrangement as rope would erase the veto filter: the coordination story is genuine, but activation runs through five national interests, and pretending otherwise launders selective use as impartial protection. Mislabeling it as snare would erase the delivered goods: Kenya 2008, Cote d'Ivoire 2011, and Gambia 2017 were real protective outcomes no pre-existing mechanism produced, and the reframing that retired the right-to-intervene language was a genuine normative accomplishment. Holding both halves in one type is what lets the corpus measure drift: the theater_ratio series is the early-warning instrument for the characteristic failure mode, in which the coordination function atrophies into annual dialogue while the extraction half (veto-gated discretion) persists untouched - a trajectory toward piton that the current data approach but have not reached, since invocation still occasionally delivers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (r2p_reading) of the kernel article_2_7_chapter_vii_tension; how would the classification shift if the same standing arrangement were instantiated under the sibling sovereignty_first_reading?',
    'Generate the sibling file over the identical scenario and interval, relocating the injury set (persecuted populations denied recourse become the aggrieved parties; targeted states regain shield status), re-derive directionalities, and classify independently; compare the two classifications as the measure of the kernel''s contest.',
    'Under the sibling reading the arrangement''s epsilon concentrates on unprotected populations rather than on targeted states and the sovereignty norm; large cross-reading divergence indicates the dispute is about the constraint''s identity, not its severity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Reading-indexicality of the classification: which reading of the Charter kernel is being measured.').

omega_variable(
    doctrine_content_vs_p5_application,
    'Is the authored extractiveness a property of the doctrine''s content (conditionality on atrocity) or of its veto-filtered application (five national interests gating every activation)?',
    'Episode-level comparison of activations that approximated neutral criteria (Kenya 2008, Cote d''Ivoire 2011) against openly instrumental invocations (Libya 2011 onward), isolating extraction attributable to trigger design from extraction attributable to veto discretion.',
    'If extraction concentrates in the authorization channel, procedural reform (standing criteria, General Assembly routing) could pull effective extraction toward the coordination floor without touching doctrine content; if intrinsic to conditionality, no procedural fix helps and the tangled-rope reading hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_content_vs_p5_application, empirical, 'Decomposition of measured extraction into doctrinal and discretionary components.').

omega_variable(
    net_beneficiary_status_of_protected_populations,
    'For the persecuted populations the arrangement nominally serves, does protective expectation deliver net benefit, or does it net harm through raised expectations, fifth-column stigmatization, and escalation accelerants before any operation arrives?',
    'Post-episode welfare tracing comparing population trajectories in invoked versus comparable non-invoked atrocity settings, controlling for baseline intensity.',
    'A net-harm finding would strip the coordination half from the intended beneficiaries and push classification toward snare; confirmation of net benefit stabilizes the tangled-rope hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_beneficiary_status_of_protected_populations, empirical, 'Whether the nominal beneficiaries actually come out ahead under the arrangement.').

omega_variable(
    transitional_vs_permanent_asymmetry,
    'Is the selective-responsibility regime a transitional stage toward a stabilized protection regime, or a durable asymmetry hardening around permanent-member discretion?',
    'Track authorization-channel reform attempts, invocation rates, and small-state counter-coalition formation across the coming decade; sustained invocation freeze with rising declaratory activity favors durability.',
    'A transitional finding would support reinterpreting the constraint as scaffold-family with an eventual sunset condition; a durable finding locks tangled-rope classification with continued theater accumulation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transitional_vs_permanent_asymmetry, conceptual, 'Lifecycle trajectory of the selective-protection arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__r2p_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(arti_tr_t5, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 5, 0.36).
narrative_ontology:measurement(arti_tr_t10, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(arti_tr_t15, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(arti_tr_t20, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement(arti_tr_t25, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 25, 0.6).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(arti_be_t5, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(arti_be_t10, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(arti_be_t15, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(arti_be_t20, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(arti_be_t25, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 25, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(arti_su_t5, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(arti_su_t10, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(arti_su_t15, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(arti_su_t20, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(arti_su_t25, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 25, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__r2p_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension__sovereignty_first_reading).

% DUAL FORMULATION NOTE:
% Colloquial usage treats 'the sovereignty/intervention question in the UN Charter' as one debate; it is two structurally distinct constraints sharing one fixed-text kernel. The sovereignty-first reading (1945 orthodoxy, consent-or-aggression-only) and the r2p reading (2001-onward conditionality) assign the same Charter clauses different victim sets and different epsilons, so measuring either with the other's observable violates epsilon-invariance. They are linked here as a constraint family: the sibling file carries the complementary classification, and cross-reading divergence is the measurement of the kernel's live contest. Upstream/downstream is ambiguous by design - the Charter text grounds both, and neither reading's evidence base settles the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
