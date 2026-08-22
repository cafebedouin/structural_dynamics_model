% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__sovereignty_first_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__sovereignty_first_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: article_2_7_chapter_vii_tension__sovereignty_first_reading
 *   human_readable: State Sovereignty First: Article 2(7) and Chapter VII Authorization Gate
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty-first reading of the Article
 *   2(7)/Chapter VII tension. The UN Charter mandates that the UN shall not
 *   intervene in matters within the domestic jurisdiction of any state
 *   (Article 2(7)), except when the Security Council determines under Chapter
 *   VII that a threat to international peace and security exists and
 *   authorizes enforcement action. The sovereignty-first reading interprets
 *   this as: (1) state consent is the default gate on intervention; (2)
 *   Chapter VII authorization is available only when one state has committed
 *   inter-state aggression; (3) internal atrocity, however severe, is not
 *   grounds for intervention without state consent or a narrow Security
 *   Council finding of international-peace threat narrowly construed. This
 *   reading protects atrocity-capable regimes from external intervention by
 *   coding internal violence as outside the international legal domain. The
 *   beneficiary set is authoritarian states and post-colonial states whose
 *   primary political project is resisting external interference. The victim
 *   set is populations experiencing domestic atrocity, whose protection
 *   requires intervention the sovereignty-first reading blocks. The kernel is
 *   contested: an alternative reading (r2p_reading) treats sovereignty as
 *   conditional on populations' protection, and codes massive atrocity as
 *   triggering international responsibility to intervene, making Chapter VII
 *   authorization available for population protection rather than inter-state
 *   aggression alone.
 *
 * KEY AGENTS:
 *   - Authoritarian state apparatus: administers the non-interference principle; benefits from legal shield against intervention
 *   - Post-colonial state consolidation: benefits from sovereignty protection against great-power domination; structural ally of atrocity states under the same doctrine
 *   - Populations under domestic atrocity: trapped; bear the extraction cost of non-intervention doctrine; blocked from external protection
 *   - Humanitarian intervention advocates: excluded from decision-making seats; bear the cost of constraint enforcement via diplomatic marginalization
 *   - Security Council permanent members: administer the Chapter VII gate; benefit from veto power to shield allied atrocity states
 *   - International legal interpreters (ICJ, UN HR bodies): observer seats; document constraint operation and structural gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.78).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.71).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "State Sovereignty First: Article 2(7) and Chapter VII Authorization Gate").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, '5d56d66b-d059-4f8e-b02a-ffe927b7c4cf').
narrative_ontology:cs_kernel_codification('5d56d66b-d059-4f8e-b02a-ffe927b7c4cf', fixed_text).
narrative_ontology:cs_authority_grounding('5d56d66b-d059-4f8e-b02a-ffe927b7c4cf', lineage).
narrative_ontology:cs_interpretation_layer_present('5d56d66b-d059-4f8e-b02a-ffe927b7c4cf').
narrative_ontology:cs_reading_relation('5d56d66b-d059-4f8e-b02a-ffe927b7c4cf', article_2_7_chapter_vii_tension__r2p_reading, forecloses).
narrative_ontology:cs_axiom('5d56d66b-d059-4f8e-b02a-ffe927b7c4cf', foundational, state_sovereignty_foundational).
narrative_ontology:cs_axiom_status(state_sovereignty_foundational, holdable).
narrative_ontology:cs_axiom_grounding('5d56d66b-d059-4f8e-b02a-ffe927b7c4cf', state_sovereignty_foundational, deontological).
narrative_ontology:cs_axiom('5d56d66b-d059-4f8e-b02a-ffe927b7c4cf', foundational, non_interference_norm_primary).
narrative_ontology:cs_axiom_status(non_interference_norm_primary, holdable).
narrative_ontology:cs_axiom_grounding('5d56d66b-d059-4f8e-b02a-ffe927b7c4cf', non_interference_norm_primary, conventional).
narrative_ontology:cs_axiom('5d56d66b-d059-4f8e-b02a-ffe927b7c4cf', secondary, chapter_vii_inter_state_aggression_only).
narrative_ontology:cs_axiom_status(chapter_vii_inter_state_aggression_only, holdable).
narrative_ontology:cs_axiom_grounding('5d56d66b-d059-4f8e-b02a-ffe927b7c4cf', chapter_vii_inter_state_aggression_only, conventional).
narrative_ontology:cs_reference_frame('5d56d66b-d059-4f8e-b02a-ffe927b7c4cf', post_wwii_westphalian_non_intervention).
narrative_ontology:cs_drift_state('5d56d66b-d059-4f8e-b02a-ffe927b7c4cf', contemporary_atrocity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5d56d66b-d059-4f8e-b02a-ffe927b7c4cf', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_state_apparatus).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_state_consolidation).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, humanitarian_intervention_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, security_council_permanent_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A state regime whose domestic governance includes systematic human rights violations, forced displacement, or ethnic atrocity. The sovereignty-first reading shields the regime from external intervention by requiring explicit UN Security Council authorization under Chapter VII (framed narrowly as inter-state aggression only). The regime administers the non-interference principle by invoking it in diplomatic forums, conditioning any humanitarian access on explicit state consent, and using the sovereignty doctrine to block investigative mechanisms and peacekeeping mandates that would expose or constrain domestic violence.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_state_apparatus, agenda_setter,
    institutional, generational, trapped, national).

% Newly independent or historically vulnerable states whose primary political project is asserting territorial integrity and freedom from external great-power intervention. The sovereignty-first reading protects this project by treating state consent as the authoritative gate on intervention, regardless of internal violence. This constituency includes non-atrocity states with legitimate concerns about intervention as a pretext for neo-colonial domination, and their interests are structurally bundled with those of atrocity regimes under the same doctrine.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_state_consolidation, beneficiary,
    institutional, generational, trapped, national).

% Civilians experiencing genocide, ethnic cleansing, forced displacement, or crimes against humanity perpetrated by their own state or state-aligned militias. The sovereignty-first reading blocks external intervention by requiring state consent or Security Council authorization narrowly limited to inter-state aggression. These populations bear the extraction—the constraint prioritizes state sovereignty over their protection, and the non-interference principle leaves them trapped within borders the state controls. Their only options are internal resistance (suppressed by the state apparatus that commits the atrocity) or external asylum seeking (which does not address the atrocity itself).
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity, payer,
    powerless, immediate, trapped, local).

% NGOs, human rights organizations, some democratic states, and international legal scholars who argue that sovereignty cannot shield atrocity—that a state's systematic violence against its own population creates an international legal obligation to intervene. The sovereignty-first reading excludes them from authoritative voice: their arguments are framed as violations of non-interference, their proposed interventions require state consent (which the atrocity state denies) or Security Council authorization (which atrocity-state allies veto). They bear the cost of constraint enforcement through diplomatic marginalization and legal obstacles to the intervention mechanisms they advocate.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, humanitarian_intervention_advocates, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__sovereignty_first_reading, humanitarian_intervention_advocates, excluded).

% The five permanent Security Council members hold veto power over any Chapter VII authorization that would expand the definition of inter-state aggression to include intra-state atrocity. Under the sovereignty-first reading, they administer the gate: they interpret what counts as a threat to international peace and security (the Article 39 threshold for Chapter VII action), and they veto expansive readings that would mandate intervention in domestic atrocities. States with close ties to atrocity regimes (China/Russia aligning with Syria/Myanmar; Western powers aligning with strategic partners) use the veto to shield those regimes. The constraint benefits them by preserving their freedom of action in ally-state affairs.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, security_council_permanent_members, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__sovereignty_first_reading, security_council_permanent_members, beneficiary).

% The International Court of Justice, treaty bodies, and the UN Human Rights Council document and interpret the constraint's operation. They testify to the state-centric reading of Article 2(7) and Chapter VII, track how the doctrine is invoked to block intervention, and increasingly record the structural gap between the sovereignty-first reading and population protection outcomes.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, international_legal_interpreters, observer,
    institutional, generational, analytical, global).

% States that rhetorically endorse humanitarian intervention and population protection, but whose Security Council veto holders face political constraints (alliance commitments, arms-sale dependencies, regional strategy) that make veto override politically costly. They are excluded from fully-voiced advocacy for intervention because veto would require overriding their own allies or economic interests. Some conduct intervention without explicit authorization, which the sovereignty-first reading frames as violation of international law.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, western_democratic_states, excluded,
    powerful, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__sovereignty_first_reading, security_council_permanent_members).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__sovereignty_first_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rule-based system in which state territorial integrity and non-interference are protected from unilateral power projection: prevents any military from invading another under humanitarian pretext, and requires international consensus (Security Council) for peace-enforcement actions. Solves the problem of preventing powerful states from colonizing weaker ones under justice rhetoric.
% TRANSFER_FUNCTION: Transfers the authority to decide on intervention from individual state judgment to the UN Security Council (requiring consensus among permanent members), and restricts the grounds for such authorization to inter-state aggression, not intra-state atrocity. This shifts who has voice in the decision: from humanitarian advocates to state executives and their Security Council representatives.
% ABSENT_VOICES: Populations experiencing atrocity have no seat at the Security Council; their survival interests are not represented in the authorization structure. Humanitarian advocates and the International Criminal Court occupy marginal observational roles, not decision-making seats. The constraint's enforcement depends on their exclusion—if atrocity survivors had a veto, the authorization threshold would shift immediately.
% DISAPPEARANCE_RATIONALE: If the sovereignty-first reading vanished and were replaced by a threshold-triggered intervention standard (atrocities above X severity auto-trigger international authority to intervene), atrocity states would scramble to hide violence, humanitarian advocates would gain standing, permanent Security Council members would lose veto leverage, and populations would have an external protection mechanism independent of consent. The international system would reorganize from sovereignty-as-shield to sovereignty-conditional-on-protection.
% FOUNDING_PROBLEM: Post-World War II, the risk of great-power military intervention under humanitarian or ideological pretext was treated as graver than the risk of state atrocity: the colonialism and proxy wars of the Cold War era showed that intervention doctrine could be weaponized. The UN Charter's Article 2(7) was designed to protect newly independent states from external domination by privileging sovereignty and non-interference as foundational, overriding potential claims about internal justice.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preventing neo-colonial intervention) was live during the Cold War and justified the non-interference doctrine. By the 1990s-2000s, the vector had substantially diminished: proxy wars ended, unilateral humanitarian intervention largely ceased, and the founding justification lost explanatory power. Historians (Krasner, Finnemore) document that the founding problem has been superseded. Simultaneously, the constraint enabled atrocities (Rwanda 1994, Syria 2011+, Myanmar 2017+) that demonstrate the founding problem is no longer the graver risk. This is mandatrophy: founding problem dead, constraint persists.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__sovereignty_first_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__sovereignty_first_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) and rising over time (0.62→0.78 across 75 years). The trajectory reflects increasing evidence of systematic atrocity (Rwanda, Syria, Myanmar, Ethiopia) occurring within the sovereignty-first frame, such that the constraint's primary function is no longer preventing neo-colonial intervention (the founding problem) but enabling atrocity-state impunity. Suppression is substantial (0.71) and rising (0.55→0.71): the constraint requires active enforcement—humanitarian advocates must be kept from proposing alternative readings, atrocity populations must be prevented from accessing international protection mechanisms, and Security Council permanent members must maintain the veto blocking expansive Chapter VII readings. Theater ratio is moderate (0.42) and rising (0.28→0.42), indicating that the constraint's operational surface increasingly consists of theatrical acts (diplomatic statements affirming non-interference, Security Council abstentions framed as respect for sovereignty, formal investigations with no enforcement) rather than genuine coordination. The measurements share one time grid, with all three metrics authored at every interval point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (atrocity states, Security Council veto-holders) experiences the constraint as legitimate protection of sovereignty and international stability. From their position, the constraint solves a real coordination problem: preventing military intervention under justice rhetoric. The victim and excluded seats (atrocity populations, humanitarian advocates) experience the same structural rule as enabling atrocity by blocking the one mechanism that could stop it. The engine should compute this divergence: high extraction and suppression from the victim/excluded perspective; low extraction (coordination cost) from the beneficiary perspective. The claim (tangled_rope) reflects the authentic dual nature: genuine coordination problem + asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian state apparatus: high d (near target end) because it benefits from the constraint but its position as agenda_setter reflects power, not escape. Actually: the regime benefits from non-interference (low extraction from its position), but is named as beneficiary because the constraint is *designed* to protect it from intervention. Directionality: the regime holds power and gains from the arrangement → d should be low (beneficiary-like) but the regime also administers the constraint as agenda_setter. Override: d=0.25 for authoritarian_state_apparatus (benefits from protection despite formal power, is captured by the constraint's logic). Post-colonial state consolidation: legitimate beneficiary (d=0.2), genuinely benefits from sovereignty protection. Populations under atrocity: maximum target (d=0.95), trapped, no exit, extracted from. Humanitarian advocates: excluded, organized, constrained exit (cannot intervene without authorization they are denied) → d=0.85. Security Council permanent members: d=0.3 (beneficiary-like: they gain from veto power, benefit from flexibility to shield allies, but are also constrained by the constraint's formal limits on their authority).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing great-power intervention under humanitarian pretext) was live for decades. The constraint solved it: states were protected from unilateral intervention without Security Council authorization. By the 1990s, the founding problem had substantially resolved (Cold War ended, proxy wars diminished, unilateral humanitarian intervention largely ceased to be the vector for great-power competition). Simultaneously, evidence accumulated that the non-intervention doctrine enabled atrocities at scale (Rwanda 1994, Bosnia 1990s, Sudan 2000s, Syria 2011+). The constraint persists despite its founding problem's death because: (1) it benefits atrocity-capable states and their allies (who veto Chapter VII expansion); (2) it benefits non-atrocity post-colonial states whose legitimate sovereignty-protection interests are bundled with atrocity regimes' shelter under the same doctrine; (3) the Security Council structure makes reform nearly impossible (China and Russia veto expansive readings; atrocity-state allies block reform). This is a textbook mandatrophy case: founding problem dead (or massively outweighed), constraint persists because beneficiaries are powerful enough to defend it, and the cost of persistence (atrocity-scale population harms) exceeds the cost of fixing it (reform the Article 2(7)/Chapter VII interpretation) by orders of magnitude. The sovereignty-first reading is the skeleton holding the corpse upright.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_outweighing,
    'Has the founding problem (preventing intervention under humanitarian pretext) been outweighed by atrocity-scale harms that non-interference enables, or do the two risks remain in balance?',
    'Comparative casualty accounting: measure deaths from interventions conducted under humanitarian pretext (Cold War proxies, post-Cold War discretionary actions) vs. deaths from atrocities enabled by non-intervention doctrine (Rwanda, Syria, Myanmar). The ratios establish whether the founding problem remains the graver risk.',
    'If outweighing is established, mandatrophy is confirmed: the constraint persists despite its founding justification having been exceeded by its harms. If risks remain balanced, the sovereignty-first reading retains structural legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_outweighing, empirical, 'Whether founding problem justification has been outweighed by atrocity-enabling harms.').

omega_variable(
    structural_bundling_of_legitimate_and_illegitimate_beneficiaries,
    'Is the sovereignty-first reading''s protection of post-colonial states'' legitimate interests (freedom from neo-colonial domination) structurally separable from its protection of atrocity regimes'' illegitimate interests (impunity for domestic violence)?',
    'Legal redesign: could a modified reading that expands Chapter VII authorization to include atrocity while preserving the veto for great-power military domination split the beneficiary set? Or is the non-interference doctrine indivisible?',
    'If separable, reform could protect post-colonial sovereignty from neo-colonialism while enabling intervention against atrocity. If inseparable, post-colonial states'' legitimate interests are weaponized to shield atrocity, making the constraint harder to reform without harming states with justified non-intervention concerns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_bundling_of_legitimate_and_illegitimate_beneficiaries, conceptual, 'Whether sovereignty protection for post-colonial states can be decoupled from atrocity-state impunity.').

omega_variable(
    veto_holder_alignment_with_atrocity_states,
    'To what extent does Security Council veto-holder protection of atrocity states reflect genuine belief in the sovereignty-first reading vs. strategic alliance-maintenance?',
    'Comparative analysis of veto patterns: do veto holders block expansive Chapter VII readings consistently across all atrocity cases, or selectively (protecting allies, not others)? Diplomatic archive analysis and Security Council voting records.',
    'If consistent, the veto reflects principled application of the sovereignty-first reading. If selective, the reading is a pretext for strategic interest, and the constraint''s legitimacy derives from power, not doctrine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_holder_alignment_with_atrocity_states, empirical, 'Whether veto patterns reflect principled reading interpretation or strategic alliance maintenance.').

omega_variable(
    reading_foreclosure_or_coexistence,
    'Does the sovereignty-first reading logically foreclose the r2p reading within a single legal framework, or do they coexist as competing live interpretations?',
    'Jurisprudential analysis: can both readings be held consistent with the UN Charter''s text, or does accepting one necessarily reject the other at the level of core premises?',
    'If foreclosure, only one can be the official interpretation; legal reform would require explicit choice between readings, making the contest a binary decision point. If coexistence, the contest is a chronic competition for institutional authority, amenable to gradual rebalancing without formal amendment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'Whether readings logically foreclose each other or coexist as live alternatives.').

omega_variable(
    extraction_mechanism_transparency,
    'Is the extraction by atrocity states from populations grounded in the sovereignty-first reading explicit and acknowledged, or obscured by framing sovereignty as protective rather than extractive?',
    'Discourse analysis and stakeholder interview: do atrocity-state officials, Security Council members blocking intervention, and sovereignty traditionalists acknowledge that non-interference enables atrocity, or do they deny/obscure that connection?',
    'If extraction is transparent, it becomes harder to defend the reading on justice grounds. If extraction is obscured by legitimate-sounding language (sovereignty, stability, preventing neo-colonialism), the constraint''s persistence is more robust against critique.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_transparency, empirical, 'Whether the reading''s extractive function is transparent or obscured by protective rhetoric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(arti_tr_t10, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(arti_tr_t20, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(arti_tr_t30, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(arti_tr_t45, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 45, 0.4).
narrative_ontology:measurement(arti_tr_t60, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement(arti_tr_t75, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 75, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(arti_be_t10, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(arti_be_t20, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement(arti_be_t30, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 30, 0.74).
narrative_ontology:measurement(arti_be_t45, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 45, 0.76).
narrative_ontology:measurement(arti_be_t60, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 60, 0.77).
narrative_ontology:measurement(arti_be_t75, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 75, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(arti_su_t10, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement(arti_su_t20, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(arti_su_t30, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(arti_su_t45, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 45, 0.69).
narrative_ontology:measurement(arti_su_t60, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(arti_su_t75, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 75, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__sovereignty_first_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.15).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension__r2p_reading).

% DUAL FORMULATION NOTE:
% This story and article_2_7_chapter_vii_tension__r2p_reading are sibling readings of the article_2_7_chapter_vii_tension kernel. They are separate constraints with different ε values, beneficiary/victim structures, and CS readings. The sovereignty-first reading codes intervention-blocking as the primary function (high ε for populations); r2p codes atrocity-enabling as the primary cost (high ε for atrocity states under r2p framing). The readings are linked via cs_structure.reading_relations; each is independently ε-invariant per its own framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_2_7_chapter_vii_tension__sovereignty_first_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
