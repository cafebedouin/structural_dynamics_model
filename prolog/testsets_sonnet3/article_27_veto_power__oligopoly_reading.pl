% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__oligopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__oligopoly_reading, []).

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
 *   constraint_id: article_27_veto_power__oligopoly_reading
 *   human_readable: UN Security Council P5 Veto (Article 27) as Entrenched Oligopoly
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   The UN Security Council's permanent five members (US, Russia, China, UK,
 *   France) hold an unconditional veto under Article 27(3) that has never
 *   been substantively reformed since 1945, and Article 108's unanimity
 *   requirement for Charter amendment means the P5 control the only legal
 *   path to changing their own privilege. Eighty years of decolonization, the
 *   rise of new economic and demographic powers, and repeated reform
 *   initiatives (Ezulwini Consensus, G4, Uniting for Consensus, the 2022
 *   veto-initiative resolution) have produced procedural tweaks (mandatory
 *   explanation speeches) but zero structural change. This reading treats
 *   that persistence not as evidence of a well-functioning coordination
 *   mechanism but as evidence of a self-perpetuating rent: the P5 extract
 *   ongoing authority — agenda control, immunity from binding Council action
 *   against their interests, veto-shielded allies — while the amendment
 *   mechanism that could correct the imbalance is itself vetoable by the same
 *   five parties.
 *
 * KEY AGENTS:
 *   - p5_permanent_members: agenda_setter/beneficiary (institutional/arbitrage) — sets Council agenda, blocks amendment, collects ongoing authority rent
 *   - non_p5_un_member_states: payer (moderate/trapped) — bears permanent exclusion with no amendment path
 *   - elected_security_council_members: payer (moderate/constrained) — temporary voice, no durable power, votes nullifiable by single P5 veto
 *   - global_south_coalition: payer/excluded (organized/trapped) — largest population share, structurally locked out of both veto and amendment mechanism
 *   - un_secretariat_and_charter_scholars: observer (analytical) — documents the deadlock pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.81).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.88).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "UN Security Council P5 Veto (Article 27) as Entrenched Oligopoly").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, '878ce914-59cb-4787-9b8d-74c974db3b2c').
narrative_ontology:cs_kernel_codification('878ce914-59cb-4787-9b8d-74c974db3b2c', fixed_text).
narrative_ontology:cs_authority_grounding('878ce914-59cb-4787-9b8d-74c974db3b2c', extraction).
narrative_ontology:cs_interpretation_layer_present('878ce914-59cb-4787-9b8d-74c974db3b2c').
narrative_ontology:cs_reading_relation('878ce914-59cb-4787-9b8d-74c974db3b2c', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('878ce914-59cb-4787-9b8d-74c974db3b2c', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('878ce914-59cb-4787-9b8d-74c974db3b2c', foundational, entrenched_privilege_requires_ongoing_justification).
narrative_ontology:cs_axiom_status(entrenched_privilege_requires_ongoing_justification, holdable).
narrative_ontology:cs_axiom_grounding('878ce914-59cb-4787-9b8d-74c974db3b2c', entrenched_privilege_requires_ongoing_justification, empirically_contingent).
narrative_ontology:cs_axiom('878ce914-59cb-4787-9b8d-74c974db3b2c', foundational, self_amending_veto_over_own_removal_is_illegitimate_lock_in).
narrative_ontology:cs_axiom_status(self_amending_veto_over_own_removal_is_illegitimate_lock_in, holdable).
narrative_ontology:cs_axiom_grounding('878ce914-59cb-4787-9b8d-74c974db3b2c', self_amending_veto_over_own_removal_is_illegitimate_lock_in, deontological).
narrative_ontology:cs_reference_frame('878ce914-59cb-4787-9b8d-74c974db3b2c', id_1945_victors_permanent_privilege).
narrative_ontology:cs_drift_state('878ce914-59cb-4787-9b8d-74c974db3b2c', contemporary_multipolar_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('878ce914-59cb-4787-9b8d-74c974db3b2c', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, p5_permanent_members).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_p5_un_member_states).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, elected_security_council_members).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, global_south_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The United States, Russia, China, the United Kingdom, and France hold a permanent seat and an unconditional veto over any non-procedural Security Council resolution, codified in Article 27(3) of the Charter. They also control the amendment path itself: Article 108 requires any Charter amendment to be ratified by all five, meaning they can block any reform that would dilute their own privilege. They set the agenda for what counts as a legitimate Council action and collect ongoing diplomatic and material rents from being the fixed arbiters of collective security.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, p5_permanent_members, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, p5_permanent_members, beneficiary).

% The 188 non-permanent UN member states have no structural path to veto power regardless of population, economic weight, or nuclear status acquired after 1945. They can propose reform resolutions in the General Assembly, but any Charter amendment affecting the veto requires P5 ratification, which none has ever granted. Exit from the UN system means forfeiting the only near-universal forum for collective legitimacy, so departure is not a real option; participation without power is the sustained cost.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, non_p5_un_member_states, payer,
    moderate, civilizational, trapped, global).

% Ten states elected for two-year terms sit at the table and vote on resolutions but hold no veto; their votes can be nullified by a single P5 negative vote regardless of the other fourteen members' positions. They gain visibility and negotiating leverage during their term but exit that leverage entirely when their term ends, having built no durable institutional power.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, elected_security_council_members, payer,
    moderate, biographical, constrained, global).

% Africa, Latin America, and much of Asia have no permanent representation at all despite comprising the large majority of the world's population and UN membership. Decades of coordinated reform proposals (the Ezulwini Consensus, the G4 bid, the Uniting for Consensus bloc) have produced no Charter change. They are structurally excluded from the amendment-blocking mechanism itself — they can petition but cannot compel.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, global_south_coalition, payer,
    organized, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, global_south_coalition, excluded).

% Legal scholars, former diplomats, and secretariat officials document the reform deadlock, the frequency and pattern of vetoes, and the Charter's internal self-entrenchment mechanism. They have no power to compel reform but produce the record against which the oligopoly reading is assessed.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, un_secretariat_and_charter_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__oligopoly_reading, p5_permanent_members).
narrative_ontology:fixing_cost_class(article_27_veto_power__oligopoly_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The veto nominally coordinates great-power consent so that the Council cannot authorize action a nuclear-armed permanent member would resist by force — a genuine problem in 1945 given the alternative (League of Nations collapse from great-power defection).
% TRANSFER_FUNCTION: The arrangement moves durable authority — agenda control, legal legitimation of force, sanctions design, and diplomatic leverage — from the broader UN membership to five fixed states, and moves the cost of reform-blocking (perpetual underrepresentation, unaddressed grievances, institutional paralysis on issues touching P5 interests) onto the non-P5 majority.
% ABSENT_VOICES: States that did not exist or were colonized in 1945 — most of Africa, much of Asia — had no seat at the founding conference where the veto was fixed into the Charter. Today's rising powers (India, Brazil, Nigeria, a unified African seat) argue for permanent representation but are structurally excluded from the Article 108 amendment path that could grant it.
% DISAPPEARANCE_RATIONALE: If the veto disappeared overnight, Security Council resolutions would pass or fail on majority vote alone; the P5 would lose their unilateral blocking power over interventions, sanctions regimes, and tribunal referrals touching their own interests or allies, and decades-stalled reform proposals (Ezulwini, G4, Uniting for Consensus) would suddenly have a viable path forward — the entire architecture of who can compel or block collective security action would reorganize.
% FOUNDING_PROBLEM: In 1945, the founders sought to avoid the League of Nations' fatal flaw: a collective security body that great powers could simply exit or ignore when it constrained them, which had already failed to prevent Japanese, Italian, and German aggression. The veto was designed to keep the five wartime victors inside the institution by guaranteeing none could be outvoted into a war it did not choose.
% FOUNDING_PROBLEM_CORROBORATION: The P5 attest the founding problem remains live, citing continued great-power military capacity and the risk of Council-authorized confrontation between nuclear states. Independent Charter scholars, the Ezulwini Consensus signatories, and successive UN Secretary-General reform panels (most recently the 2022 veto-initiative resolution requiring P5 to explain vetoes to the General Assembly) attest from outside the P5 that the original prevent-great-power-defection rationale has been substantially decoupled from the veto's actual contemporary use, which increasingly shields P5 allies and P5 own conduct from accountability rather than preventing a founding-era great-power war.
narrative_ontology:disappearance_verdict(article_27_veto_power__oligopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__oligopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__oligopoly_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__oligopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__oligopoly_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__oligopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_27_veto_power__oligopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) and rising over the 80-year interval because the gap between the P5's 1945 share of global power/population/economy and their 2020s share has widened while their Charter privilege has not adjusted — this is rent extraction that compounds with time rather than a stable coordination cost. Suppression is authored even higher (0.88) because the defining structural feature of this reading is that the mechanism for correcting the imbalance (Article 108 amendment) is itself captured by the same veto holders — suppression here is not merely resistance to change but resistance built into the amendment procedure itself, which is a stronger and more diagnostic form of suppression than ordinary institutional inertia. Theater ratio is moderate and rising (0.10 to 0.42) reflecting the growing gap between reform theater (annual General Assembly debates, the 2022 veto-initiative resolution requiring explanation) and actual structural change (zero amendments to Articles 23/27/108 since 1965's non-permanent-seat expansion, which did not touch the veto itself).
 *
 * DIRECTIONALITY LOGIC:
 *   P5 states are the clear structural beneficiaries: they set the rules, benefit from the extraction, and hold arbitrage-grade exit (they can act unilaterally or through coalitions of the willing when the Council is deadlocked, while non-P5 states cannot). Non-P5 states, elected members, and the Global South coalition are targets: their exit options range from constrained (elected members, who at least get a temporary seat) to trapped (non-P5 majority and Global South states, who have no seat and no amendment leverage at all). The directionality gradient tracks almost exactly with proximity to the amendment-blocking mechanism — the closer a state's fate is tied to Article 108's unanimity requirement, the higher its effective extraction under this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (keep victorious great powers inside the postwar order rather than let them defect as in the League of Nations) was arguably live in 1945-1991 when the P5 were also the five nuclear-weapons states most capable of derailing collective security by force. This reading holds that the problem's contemporary status is contested-trending-dead: several P5 members' relative power has been overtaken by non-P5 states (economically, demographically, or in some domains militarily) while the veto persists unchanged, and the veto is now documented (Syria, Ukraine-related resolutions, various Israel-Palestine resolutions) being used primarily to shield allies or the P5 member's own conduct rather than to prevent a founding-era great-power confrontation. Classifying this as Snare rather than Tangled Rope requires that the coordination story be assessed as cover rather than as a genuinely still-functioning joint benefit — this reading takes that position; the coordination_reading sibling takes the opposite position on the same textual provision, which is exactly the kind of divergence the kernel framework is built to hold without forcing a single verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_function_genuinely_decoupled,
    'Has the veto''s original war-prevention coordination function become fully decoupled from its contemporary use, or does it still perform meaningful war-prevention work that would be lost if removed?',
    'Comparative analysis of veto exercises 1946-1991 versus 1991-present, coded by whether the blocked resolution would plausibly have triggered direct P5-vs-P5 military confrontation versus whether it targeted a P5 ally or the P5 member''s own conduct. A high proportion of the latter in recent decades would support the decoupling claim central to this reading.',
    'If the coordination function is shown to be substantially decoupled, this reading''s snare classification is strengthened and the coordination_reading sibling''s tangled_rope/rope classification becomes harder to sustain on the same evidentiary record. If the function persists robustly, the sibling readings gain support and this reading''s ε may be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_genuinely_decoupled, empirical, 'Whether the veto''s coordination rationale still holds empirically or has become a cover story.').

omega_variable(
    amendment_path_genuinely_foreclosed,
    'Is Article 108''s unanimity requirement a genuinely immovable structural fact, or could sustained multilateral pressure (e.g., a General Assembly-driven customary reinterpretation, as occurred with the 1971 China seat transfer) eventually force reform without formal amendment?',
    'Historical precedent analysis: the 1971 transfer of the China seat from Taipei to Beijing occurred via General Assembly resolution and Council practice without a formal Article 108 amendment, suggesting some paths around the formal amendment lock exist. Track whether comparable informal mechanisms have been attempted or could be attempted for veto reform specifically.',
    'If informal reform paths exist and are viable, the suppression score and the ''no path to reform'' victim framing central to this reading would need revision downward; if the China-seat precedent is sui generis and inapplicable to veto reform itself, the prohibitive fixing_cost and trapped exit options for non-P5 states are reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amendment_path_genuinely_foreclosed, conceptual, 'Whether the formal amendment lock is the true barrier or whether informal reform paths are underexplored.').

omega_variable(
    which_kernel_framing_is_load_bearing,
    'Is the load-bearing structural fact the veto power itself (Article 27) or the self-amending entrenchment mechanism (Article 108 requiring P5 unanimity to change Article 27)? These are analytically separable: a veto without a self-entrenching amendment lock would be a very different constraint than the current combination.',
    'Compare this reading''s ε and classification against a hypothetical constraint isolating only Article 27 (the veto in isolation, assuming a majoritarian amendment path existed) versus one isolating only Article 108 (the entrenchment mechanism applied to a hypothetical non-veto privilege). This story treats both as jointly constitutive of the oligopoly reading; a decomposition test would clarify which component carries more of the extraction.',
    'If Article 108 alone (not the veto itself) is the primary driver of the snare classification, that would suggest the coordination_reading''s charitable account of the veto''s war-prevention function could be correct while still endorsing reform of the amendment procedure alone — a possible synthesis position not captured by any of the three current readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_kernel_framing_is_load_bearing, conceptual, 'Whether the veto or the amendment-lock is the more load-bearing structural element of the oligopoly reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__oligopoly_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(arti_tr_t16, article_27_veto_power__oligopoly_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(arti_tr_t32, article_27_veto_power__oligopoly_reading, theater_ratio, 32, 0.22).
narrative_ontology:measurement(arti_tr_t48, article_27_veto_power__oligopoly_reading, theater_ratio, 48, 0.3).
narrative_ontology:measurement(arti_tr_t64, article_27_veto_power__oligopoly_reading, theater_ratio, 64, 0.37).
narrative_ontology:measurement(arti_tr_t80, article_27_veto_power__oligopoly_reading, theater_ratio, 80, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__oligopoly_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(arti_be_t16, article_27_veto_power__oligopoly_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(arti_be_t32, article_27_veto_power__oligopoly_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(arti_be_t48, article_27_veto_power__oligopoly_reading, base_extractiveness, 48, 0.72).
narrative_ontology:measurement(arti_be_t64, article_27_veto_power__oligopoly_reading, base_extractiveness, 64, 0.77).
narrative_ontology:measurement(arti_be_t80, article_27_veto_power__oligopoly_reading, base_extractiveness, 80, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_27_veto_power__oligopoly_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(arti_su_t16, article_27_veto_power__oligopoly_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(arti_su_t32, article_27_veto_power__oligopoly_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(arti_su_t48, article_27_veto_power__oligopoly_reading, suppression_requirement, 48, 0.8).
narrative_ontology:measurement(arti_su_t64, article_27_veto_power__oligopoly_reading, suppression_requirement, 64, 0.85).
narrative_ontology:measurement(arti_su_t80, article_27_veto_power__oligopoly_reading, suppression_requirement, 80, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__oligopoly_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints reading the same kernel (article_27_veto_power): the coordination_reading treats the veto as a war-prevention mechanism (lower ε, tangled_rope/rope-leaning), the sovereignty_reading treats it as a Westphalian consent principle for great powers (mountain-adjacent framing around binding-law-without-consent), and this oligopoly_reading treats it as entrenched extraction (snare, ε=0.81). All three share the same textual provision (UN Charter Articles 23, 27, 108) but diverge sharply on beneficiary/victim structure, coordination-function assessment, and classification. Per the ε-invariance principle, these are three distinct constraints rather than one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
