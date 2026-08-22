% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__palestinian_autochthony_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: territorial_legitimacy_dual__palestinian_autochthony_reading
 *   human_readable: Palestinian Territorial Legitimacy: Autochthony, Displacement, and Right of Return
 *   domain: political/territorial/international
 *
 * SUMMARY:
 *   This constraint story instantiates the Palestinian autochthony reading of
 *   the contested kernel 'territorial legitimacy dual.' The reading grounds
 *   Palestinian territorial claims in continuous pre-1948 habitation, the
 *   trauma and ongoing injustice of displacement, and the inalienable right
 *   of refugees to return to ancestral land and reclaim property. Under this
 *   reading, the 1948 Israeli state formation and subsequent territorial
 *   arrangement is read as an externally imposed displacement of an
 *   indigenous population; the legitimacy of the Israeli state is contested
 *   (justified by diaspora refuge-seeking, not by Palestinian consent or
 *   waiver); and territorial confinement of Palestinians is classified as
 *   extraction and oppression, not coordination or legitimate security. The
 *   constraint is the arrangement itself: the barring of Palestinian return,
 *   the territorial reduction to fragmented enclaves, the military
 *   occupation, and the legal frameworks (Law of Return, Law of Absentees,
 *   settlement rights) that crystallize exclusive Israeli control and
 *   Palestinian deprivation.
 *
 * KEY AGENTS:
 *   - Palestinian refugees in diaspora: primary victims of displacement and return denial; claim intergenerational right to land and reparations
 *   - Palestinian populations in West Bank and Gaza: secondary victims bearing ongoing territorial confinement, administrative control, and resource deprivation
 *   - Israeli state apparatus: agenda-setter enforcing territorial arrangement and exclusion; maintains monopoly on security doctrine and settlement authorization
 *   - Jewish diaspora and settler population: beneficiaries of exclusive return rights and settlement claims; protected by Israeli legal frameworks unavailable to Palestinians
 *   - International community observers: document violation of international law and humanitarian standards but lack enforcement mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.89).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.91).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.87).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Palestinian Territorial Legitimacy: Autochthony, Displacement, and Right of Return").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political/territorial/international").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, '730ff479-06bc-4352-8894-c269904f0785').
narrative_ontology:cs_kernel_codification('730ff479-06bc-4352-8894-c269904f0785', distributed).
narrative_ontology:cs_authority_grounding('730ff479-06bc-4352-8894-c269904f0785', distributed).
narrative_ontology:cs_reading_relation('730ff479-06bc-4352-8894-c269904f0785', territorial_legitimacy_dual__zionist_refuge_reading, forecloses).
narrative_ontology:cs_reading_relation('730ff479-06bc-4352-8894-c269904f0785', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('730ff479-06bc-4352-8894-c269904f0785', foundational, indigenous_autochthony_foundational).
narrative_ontology:cs_axiom_status(indigenous_autochthony_foundational, holdable).
narrative_ontology:cs_axiom_grounding('730ff479-06bc-4352-8894-c269904f0785', indigenous_autochthony_foundational, deontological).
narrative_ontology:cs_axiom('730ff479-06bc-4352-8894-c269904f0785', foundational, displacement_ongoing_extraction).
narrative_ontology:cs_axiom_status(displacement_ongoing_extraction, holdable).
narrative_ontology:cs_axiom_grounding('730ff479-06bc-4352-8894-c269904f0785', displacement_ongoing_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('730ff479-06bc-4352-8894-c269904f0785', palestinian_autochthony_precolonial_framework).
narrative_ontology:cs_drift_state('730ff479-06bc-4352-8894-c269904f0785', contemporary_post_1948, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('730ff479-06bc-4352-8894-c269904f0785', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees_diaspora).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_west_bank_population).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_gaza_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, jewish_diaspora_claimants).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_settler_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dispersed across Lebanon, Syria, Jordan, and globally after 1948 displacement and subsequent conflicts. Legally barred from return by Israeli law; economically marginalized in host countries; maintain collective memory of villages and property left behind. Inheritance of displacement trauma passes generationally. Physical return is structurally impossible within current territorial arrangement; legal pathways are absent by design of the occupying authority.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees_diaspora, payer,
    powerless, generational, trapped, global).

% Live under military administration and partial Palestinian Authority governance in a fragmentary territory surrounded by Israeli settlements and security barriers. Movement is restricted by checkpoints; land is continually expropriated for settlement expansion; water and resource access is allocated asymmetrically. Economic opportunities are severely limited; political voice is mediated through a Palestinian Authority with constrained sovereignty.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_west_bank_population, payer,
    moderate, biographical, constrained, regional).

% Live in a densely populated enclave under blockade, with severely restricted exit, limited freshwater, electrical grid failures, and periodic military operations. Exit to Egypt is administratively impossible except in rare medical cases. Civilian population is subject to recurring military targeting justified by security doctrine; humanitarian conditions deteriorate cyclically. No territorial expansion is possible; no access to West Bank; no meaningful sovereignty.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_gaza_population, payer,
    powerless, immediate, trapped, local).

% Administers the territorial arrangement and enforces Palestinian territorial confinement through military occupation, settlement policy, legal frameworks (including the Law of Return for Jewish diaspora, Law of Absentees confiscating Palestinian property, Nationality Law conditioning citizenship on non-Palestinian ethnicity), and security doctrines. Justifies the arrangement as necessary for Jewish security; maintains exclusive claims to sovereignty over the territory and veto power over Palestinian territorial expansion or return.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, regional).

% Have statutory right of return to Israel under the Law of Return (1950), granting automatic citizenship and property restitution rights. Can freely immigrate, acquire land, and establish residence without territorial constraint. Benefit from the exclusion of Palestinian return and from the territorial arrangement that provides exclusive Jewish settlement rights.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, jewish_diaspora_claimants, beneficiary,
    powerful, generational, arbitrage, global).

% Occupy West Bank settlements on expropriated Palestinian land, building homes, establishing communities, and expanding territorial footprint with state backing. Benefit from legal land acquisition frameworks, military protection, and infrastructure investment unavailable to Palestinians. Exit from settlements is possible but ideologically resisted; political power within Israeli governance is substantial.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_settler_community, beneficiary,
    powerful, generational, constrained, regional).

% Formally governs parts of the West Bank but lacks sovereignty over security, borders, water, airspace, or resources. Would argue for territorial consolidation, removal of settlements, and implementation of Palestinian right of return, but is structurally excluded from enforcement of these claims by Israeli military and political dominance. Authority functions as a controlled administrator of Israeli policy, not a co-sovereign.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_national_authority, excluded,
    moderate, generational, constrained, regional).

% United Nations, human rights organizations, and international law experts document the arrangement as a territorial deprivation of a protected population. International legal frameworks (UNRWA mandate, ICC jurisdiction, UNCHR resolutions) recognize Palestinian territorial claims and right of return, but lack enforcement mechanisms against a militarily dominant regional power with strategic allies.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, international_community_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__palestinian_autochthony_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading does not recognize a legitimate coordination function in the current arrangement. The constraint is read as pure territorial appropriation and displacement enforcement, not as coordination solving a genuine collective-action problem. Palestinian territorial confinement does not coordinate Palestinian and Israeli interests — it enforces Israeli territorial dominance and Palestinian deprivation.
% TRANSFER_FUNCTION: Moves territorial control, resources (water, land, building rights), freedom of movement, and political agency from Palestinian hands to Israeli state and Jewish settler control. The 1948 displacement and subsequent territorial reduction transfer Palestinian property and autonomy to Israeli sovereignty and settlement. Refugee populations transfer their legal status to permanent liminality — they retain historical claim but no embodied rights.
% ABSENT_VOICES: Palestinian refugees in the diaspora have no seat at negotiation tables; their material conditions and intergenerational trauma are often treated as historical fact rather than ongoing injustice. Palestinian communities in Gaza are excluded from almost all diplomatic processes. Israeli political voices that endorse Palestinian right of return exist but are marginalized within Israeli institutional frameworks. International voices calling for full territorial restoration are constrained by geopolitical realism.
% DISAPPEARANCE_RATIONALE: If this constraint — the territorial confinement of Palestinian populations and the barring of refugee return — were to disappear, Palestinian political agency would expand massively: refugee populations would have embodied access to historical land and compensation; territorial control would revert to contiguous Palestinian governance; demography and settlement patterns would shift. The Israeli state's territorial guarantee and settlement justification would collapse. The world would reorganize around a fundamentally different sovereignty arrangement.
% FOUNDING_PROBLEM: Palestinian displacement beginning in 1948 as a consequence of the establishment of the Jewish state and ongoing Israeli military occupation. The founding problem is framed by this reading as: how does a persecuted diaspora justify displacing and permanently confining an indigenous population?
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations (Amnesty International, Human Rights Watch, B'Tselem), UN fact-finding missions, Palestinian civil society, and historical scholarship on the Nakba document the founding problem as an active, unresolved territorial and refugee crisis. Israeli human rights organizations (Gisha, Breaking the Silence, Peace Now) and international legal scholars recognize the founding problem's persistence. The founding problem is live because the displacement continues (through settlement expansion, land confiscation, and return denial) and the trauma remains embodied across three generations of Palestinian refugees.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__palestinian_autochthony_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__palestinian_autochthony_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__palestinian_autochthony_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.89 across the interval, with minor fluctuation) because the constraint transfers territorial control, resources, mobility rights, and political agency away from Palestinians. The deprivation is not a side effect but the structural purpose: Palestinian territory is reduced to scattered administrative zones; water, airspace, and borders are Israeli-controlled; settlement expansion continually shrinks Palestinian land; return is legally barred. Suppression is consistently very high (0.87–0.91) because maintaining this territorial arrangement requires continuous military administration, law enforcement, and resistance to Palestinian claims — the constraint depends entirely on coercion, not on voluntary Palestinian acceptance or preference equilibration. Theater ratio is low (0.05–0.22) because most enforcement is direct and undisguised: military occupation is explicit; settlement policy is formal; the Law of Return is statutory. The small theater component reflects diplomatic language about 'security needs' and 'two-state solutions,' which provide rhetorical cover for enforcement but do not obscure the structural arrangement. Accessibility collapse is high (0.78) because alternative territorial arrangements are not visible within the constraint's enforcing institutions — Palestinian voices calling for full return and territorial consolidation are excluded from institutional channels. Resistance is high (0.87) because Palestinians and international actors continuously challenge the arrangement's legitimacy; the constraint persists not because resistance is absent but because the coercive apparatus is stronger.
 *
 * PERSPECTIVAL GAP:
 *   The seat-level divergence is extreme and structural. From the Israeli institutional and Jewish diaspora seats, the arrangement provides security and a historic refuge for a persecuted people; from this reading's framing, that narrative is a cover story for colonial displacement and ongoing extraction. From the Palestinian victim seats, the constraint is experienced as permanent dispossession and oppression; from the beneficiary seats, it is a necessary arrangement. The engine computes this divergence from the stakeholder power atoms, exit options (trapped vs. arbitrage), and victim/beneficiary declarations: a powerless, trapped refugee population (high d → high χ) faces a snare; a beneficiary with arbitrage exit (low d → negative χ) experiences coordination or subsidy. This reading authorizes those computations by explicitly naming the victim/beneficiary structure and power asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian refugees in the diaspora are trapped (no embodied territory, no legal return pathway, no alternative refuge that grants full rights) — directionality approaches 1.0 (full target). Palestinian populations in the West Bank and Gaza are constrained (some territorial base, Palestinian Authority administrative presence, but under Israeli military occupation and resource control) — directionality sits near 0.75–0.85 (heavily targeted). Jewish diaspora and the Israeli state are beneficiaries with arbitrage: the Israeli state can alter policy (though it faces political constraints internally); Jewish diaspora benefit from the Law of Return and settlement-friendly legal frameworks while maintaining option of exit to other countries — directionality near 0.0–0.15 (subsidized). The international community sits as analytical observer: they have voice but not enforcement power. This reading's assignment of victimhood to Palestinian populations and beneficiary status to Israeli state and Jewish diaspora follows directly from the autochthony principle: those with ancestral claim and continuous habitation are the rightful possessors; those who displaced them are extractors.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's classification as snare (not tangled rope) rests on the core structural claim: the arrangement has no legitimate coordination function, only extraction. A tangled rope claim would require identifying a genuine collective-action problem that both Israelis and Palestinians solve through the current territorial arrangement — for example, if the arrangement prevented worse conflict or delivered security benefits to both sides. This reading rejects that premise: it argues the arrangement was imposed without Palestinian consent and persists through coercion, not through equilibrium benefit. The founding_problem_status=live verdict (the displacement problem persists) combined with disappearance_verdict=world_rearranges (the arrangement's removal would reorganize the world fundamentally, not leave it unchanged) confirms that this is not a mountain of physics or a rope of genuine coordination. The mandatrophy resolution is absent because this reading does not claim the arrangement's mandate has expired while the structure persists — rather, it claims the mandate was never legitimate and the structure persists entirely through enforcement. A future state where the arrangement persists without active enforcement (theatrical maintenance only) would signal piton status; the current measurement of sustained high suppression indicates ongoing coercion, not degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autochthony_vs_diaspora_refuge_kernel_contest,
    'Which territorial principle is foundational: indigenous autochthony (prior continuous habitation) or diaspora refuge-seeking (historical persecution necessitating safe haven)?',
    'This is not empirically resolvable — it is a foundational value choice about which historical injustice takes precedence. Resolution would require a normative commitment external to factual analysis: either indigenous displacement is the greater injustice (autochthony side) or diaspora statelessness is the greater injustice (refuge side).',
    'Choosing autochthony as foundational inverts the entire victim/beneficiary structure: Palestinians become the primary victims, Jews become the displaced aggressors. Choosing refuge as foundational reverses it. The kernel contest is decided by this value choice, not by data.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autochthony_vs_diaspora_refuge_kernel_contest, preference, 'Which historical injustice is normatively primary: indigenous displacement or diaspora persecution?').

omega_variable(
    right_of_return_scope_ambiguity,
    'Does Palestinian right of return apply to all refugees and their descendants (current population ~5.7 million), or only first-generation refugees (current ~750k)?',
    'International law (UNRWA mandate, UN General Assembly Resolution 194) affirms the right for original refugees and their descendants, but implementation scope is contested by Israeli authorities. Clarification would require binding international adjudication or negotiated agreement.',
    'First-generation-only interpretation produces a smaller victim set and makes material implementation more feasible; descendant-inclusive interpretation affirms multi-generational harm and produces a larger victim set. The extractiveness profile remains high either way (displacement is ongoing), but the scale of required remedy differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_scope_ambiguity, empirical, 'Scope of Palestinian right of return: first generation or descendants?').

omega_variable(
    suppression_mechanism_internalization_trajectory,
    'To what extent is Palestinian acceptance of territorial confinement structural (imposed by military force and legal barriers) versus internalized (Palestinian authorities and some constituencies accept territorial reduction as a pragmatic compromise)?',
    'Post-territorial-liberation trajectory: if suppression persists or Palestinians continue accepting territorial limits even after enforcement apparatus is removed, the internalization is partial. If Palestinian demand immediately surges for full territorial restoration upon removal of enforcement, suppression was primarily structural.',
    'If suppression is largely structural, the constraint''s coercive character is clear and justifies snare classification; if significantly internalized, the constraint''s persistence reflects partial Palestinian acceptance and might indicate a shift toward tangled-rope classification (with some parties having developed equilibrium preferences for the constrained arrangement). High suppression with low theater suggests structural suppression dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization_trajectory, empirical, 'Proportion of suppression that is structural (external barriers) versus internalized (Palestinian acceptance).').

omega_variable(
    reading_incompatibility_with_zionist_refuge,
    'Can the autochthony reading and the zionist_refuge_reading coexist in a single normative framework, or do they logically foreclose each other?',
    'A framework would need to assign priority to indigenous claims over diaspora refuge-seeking (or vice versa) — the choice itself determines whether both can coexist or one forecloses the other. If a framework says ''both autochthony and refuge deserve protection, but autochthony is prior,'' the coexistence is hierarchical, not symmetric.',
    'If readings coexist_with each other, both are live positions held by different parties and negotiation focuses on balancing competing claims. If this reading forecloses the zionist reading, any framework accepting Palestinian autochthony as primary must reject diaspora refuge-seeking as a foundational legitimacy claim. The relational choice affects how the engine models the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_incompatibility_with_zionist_refuge, conceptual, 'Logical relationship between autochthony and diaspora refuge foundational claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 1948, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1967, 0.08).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1987, 0.15).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(terr_tr_t2015, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(terr_tr_t2026, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1948, 0.85).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1967, 0.88).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1987, 0.89).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2005, 0.89).
narrative_ontology:measurement(terr_be_t2015, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2015, 0.9).
narrative_ontology:measurement(terr_be_t2026, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2026, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1948, 0.82).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1967, 0.87).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1987, 0.9).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2005, 0.91).
narrative_ontology:measurement(terr_su_t2015, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2015, 0.91).
narrative_ontology:measurement(terr_su_t2026, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2026, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.05).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__two_state_coexistence_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, settlement_expansion_constraint).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, refugee_camp_life_constraint).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, gaza_blockade_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one of three kernel readings of the contested legitimacy kernel 'territorial_legitimacy_dual.' The three readings diverge on foundational legitimacy claims (autochthony vs. refuge vs. compromise) and therefore assign different ε values and victim/beneficiary structures to the same territorial arrangement. Each reading's constraint story is independent; they are linked by network.affects_constraints to flag their kinship. The corpus should include all three readings to measure structural divergence and the engine's per-seat classification computation across the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
