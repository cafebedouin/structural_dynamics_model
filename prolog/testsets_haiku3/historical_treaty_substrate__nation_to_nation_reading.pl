% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__nation_to_nation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__nation_to_nation_reading, []).

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
 *   constraint_id: historical_treaty_substrate__nation_to_nation_reading
 *   human_readable: Nation-to-Nation Treaty Framework: Indigenous Sovereignty and Ongoing Consent
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates the nation-to-nation reading of the
 *   historical treaty substrate kernel — the framing in which Indigenous
 *   nations are recognized as international sovereigns, treaties are binding
 *   ongoing agreements subject to modern international law, and unilateral
 *   settler-state resource extraction or territorial modification constitutes
 *   treaty violation. This is distinct from the extinguishment reading (which
 *   treats treaties as completed property transactions) and the stewardship
 *   reading (which treats them as relational coexistence pacts). The
 *   nation-to-nation reading emerged most forcefully in late 20th-century
 *   international legal scholarship and Indigenous sovereignty movements,
 *   crystallized in UN Declaration on the Rights of Indigenous Peoples
 *   (2007), and has been applied in specific territorial disputes (Australian
 *   Native Title, Canadian Aboriginal Title, Inter-American Court rulings).
 *   The constraint is CLAIMED as tangled_rope because it exhibits genuine
 *   coordination (binding agreements requiring ongoing negotiation) alongside
 *   asymmetric extraction (settler-state loss of unilateral authority,
 *   Indigenous nations' constrained consent mechanism). The measurement
 *   series shows extraction stabilizing by t=25 as the international law
 *   framework was institutionalized, with theater rising as compliance
 *   mechanisms became performative rather than contested.
 *
 * KEY AGENTS:
 *   - Indigenous nations: sovereigns claiming binding treaty rights and veto over territorial/resource changes
 *   - Settler state (executive/legislative): powerful institutional actor facing loss of unilateral extraction authority
 *   - Resource extraction industries: powerful actors dependent on state unilateral grants, now constrained by consent requirements
 *   - International arbitration bodies: institutional observers with authority to interpret and enforce treaty obligations
 *   - Excluded third-party claimants: developers, settler districts, competing users with no party status in nation-to-nation negotiations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.62).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.71).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Nation-to-Nation Treaty Framework: Indigenous Sovereignty and Ongoing Consent").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, 'a97e7a0b-bd19-4116-85fa-7a5b306a9245').
narrative_ontology:cs_kernel_codification('a97e7a0b-bd19-4116-85fa-7a5b306a9245', fixed_text).
narrative_ontology:cs_authority_grounding('a97e7a0b-bd19-4116-85fa-7a5b306a9245', lineage).
narrative_ontology:cs_interpretation_layer_present('a97e7a0b-bd19-4116-85fa-7a5b306a9245').
narrative_ontology:cs_reading_relation('a97e7a0b-bd19-4116-85fa-7a5b306a9245', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('a97e7a0b-bd19-4116-85fa-7a5b306a9245', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('a97e7a0b-bd19-4116-85fa-7a5b306a9245', foundational, indigenous_nations_hold_ongoing_sovereignty).
narrative_ontology:cs_axiom_status(indigenous_nations_hold_ongoing_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('a97e7a0b-bd19-4116-85fa-7a5b306a9245', indigenous_nations_hold_ongoing_sovereignty, deontological).
narrative_ontology:cs_axiom('a97e7a0b-bd19-4116-85fa-7a5b306a9245', foundational, treaties_are_binding_international_law).
narrative_ontology:cs_axiom_status(treaties_are_binding_international_law, holdable).
narrative_ontology:cs_axiom_grounding('a97e7a0b-bd19-4116-85fa-7a5b306a9245', treaties_are_binding_international_law, conventional).
narrative_ontology:cs_reference_frame('a97e7a0b-bd19-4116-85fa-7a5b306a9245', treaty_as_sovereign_commitment).
narrative_ontology:cs_drift_state('a97e7a0b-bd19-4116-85fa-7a5b306a9245', contemporary_international_human_rights_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('a97e7a0b-bd19-4116-85fa-7a5b306a9245', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, international_treaty_law_body).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_state_unilateral_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_state_executive).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_state_legislative).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold treaty rights grounded in recognition as sovereign international parties. Under this reading, they retain territorial decision-making authority subject to ongoing consent requirements; unilateral resource extraction or boundary changes by the settler state constitute treaty violations. Their authority rests on the nation-to-nation framework, which requires continuous negotiation rather than presumed subordination. They must organize to activate the consent mechanism and invoke international law bodies, and face structural pressure to fragment consent across internal divisions.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, agenda_setter).

% Operationally constrained by ongoing consent requirements in resource and territorial decisions. Under this reading, the state no longer holds unilateral authority to exploit resources or modify boundaries — it must secure treaty-partner agreement. The historical practice of treating treaties as superseded by later unilateral legislation becomes legally untenable. The state faces litigation risk, international arbitration exposure, and loss of arbitrage-grade unilateral extraction authority.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_executive, payer,
    powerful, generational, constrained, national).

% May legislate, but under this reading legislation cannot override or reinterpret treaties without consent. Legislative sovereignty becomes constrained by international law obligations and the prior treaty commitment. Historical practice of using legislation to diminish or reinterpret treaties is delegitimized. Legislators face pressure from resource-extraction constituencies while bound by treaty obligation to Indigenous partners.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_legislative, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, settler_state_legislative, agenda_setter).

% Lose the settler state's unilateral authority to grant extraction rights on claimed territory. Under this reading, resource access requires ongoing consent from Indigenous treaty partners, or extraction becomes a treaty violation. Their access to rents depends on negotiating consent — they cannot rely on state unilateral grant. Exit options include litigation challenging the treaty framework, or geographic arbitrage to territories with weaker Indigenous sovereignty claims.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_industries, payer,
    powerful, biographical, constrained, global).

% Interpret and adjudicate treaty obligations under modern international law principles. They hold structural authority to apply this reading to specific disputes, determining whether state conduct violates treaty obligations. Their rulings translate the nation-to-nation frame into enforceable remedies — compensation, restitution, injunctive relief on resource projects.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, international_arbitration_bodies, observer,
    institutional, generational, analytical, global).

% Other settler-state actors claiming extraction or settlement rights on the same land are excluded from the treaty negotiation surface. A developer seeking to build on claimed territory, or a farming district seeking water allocation, is structurally barred from the nation-to-nation conversation — they must work through the settler state, which can no longer grant unilateral rights. Their interests are real but their standing is non-party.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, competing_territorial_claimants, excluded,
    powerful, generational, trapped, national).

% The doctrine that treaty commitments bind successor governments and remain enforceable across historical transitions is vindicated by this reading. The proposition that treaties are ongoing sovereign-to-sovereign agreements, not one-time transactions, is institutionalized in this constraint structure.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, historical_continuity_doctrine, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(historical_treaty_substrate__nation_to_nation_reading, historical_continuity_doctrine).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__nation_to_nation_reading, international_arbitration_bodies).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__nation_to_nation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a binding legal framework for territorial and resource decisions that must incorporate Indigenous nation consent, replacing the prior settler-state unilateral authority model. Coordinates the settler state's extraction and development interests with Indigenous nations' territorial sovereignty and resource stewardship obligations.
% TRANSFER_FUNCTION: Transfers decision-making authority from settler-state unilateral capacity to a shared nation-to-nation negotiation requirement. Resource extraction and territorial modification can proceed only with consent, converting unilateral rents into negotiated settlements or blocked projects. Authority to interpret and enforce treaties transfers to international arbitration bodies, constraining settler-state legislative sovereignty.
% ABSENT_VOICES: Third-party claimants (developers, settler districts, competing resource users) are structurally excluded from treaty negotiation. Environmental movements claiming standing to speak for non-human stakeholders are also typically excluded. The voices present are sovereign-to-sovereign; the absent are sub-sovereign interests and non-state ecological concerns.
% DISAPPEARANCE_RATIONALE: If this treaty framework vanished overnight, the settler state would revert to unilateral authority over resource extraction and territorial modification. Indigenous nations' ability to block or renegotiate projects would collapse unless backed by parallel domestic law (which would require separate legislative action). The resource extraction industries would immediately move forward on projects currently stalled in consent negotiations. Territorial boundaries and resource allocation would shift rapidly toward settler-state preference.
% FOUNDING_PROBLEM: Settler states historically treated treaties as completed transactions that could be unilaterally reinterpreted, superseded by later legislation, or abandoned as inconvenient. Indigenous nations were left with no enforceable mechanism to block resource extraction or territorial modification. The problem this reading was built to solve: create a binding, ongoing, internationally-enforceable constraint on unilateral settler-state action.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies (UN Permanent Forum on Indigenous Issues, International Court of Justice rulings), independent legal scholars outside settler-state institutional hierarchies, and Indigenous nations' own advocates all attest that unilateral settler-state treaty reinterpretation remains a live practice. Specific corroboration: Australia's Mabo case reversal of the terra nullius doctrine, Canadian Supreme Court recognition of Aboriginal title in Delgamuukw and Haida Gwaii decisions, and Inter-American Court of Human Rights rulings on Indigenous land rights all affirm this reading's founding problem remains active.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__nation_to_nation_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__nation_to_nation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__nation_to_nation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(historical_treaty_substrate__nation_to_nation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__nation_to_nation_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the nation-to-nation frame transfers authority from settler-state unilateral capacity to shared negotiation, but that transfer is still actively contested — many settler-state actors maintain that treaties are superseded or subordinate to domestic law. Suppression is high (0.71) because the constraint's persistence depends on Indigenous nations organizing to invoke it (litigation, treaty body complaints, withholding consent) against institutional and resource-extraction opposition; suppression requirement measures the enforcement activity needed to maintain the nation-to-nation frame against the default settler-state reversion to unilateralism. Theater is moderate-high (0.58) because compliance involves ceremonial treaty acknowledgments and consultation processes that sometimes substitute for genuine consent-sharing; the settler state performs the nation-to-nation frame while maintaining de facto extraction authority through procedural capture (consultation without veto, token representation on decision boards). Accessibility collapse is moderate (0.48): alternatives to the nation-to-nation frame remain live (extinguishment reading, stewardship reading, unilateral settler-state authority) — the constraint has not fully collapsed alternatives, which is appropriate for a contested reading in active legal and political dispute. Resistance is high (0.67) because resource extraction industries, settler-state agencies, and nationalist movements actively resist the constraint's application, litigating specific treaties, claiming superior state sovereignty, or arguing for legislative override.
 *
 * PERSPECTIVAL GAP:
 *   The Indigenous nations seat and the settler-state seat compute fundamentally different classifications of this same constraint. From the Indigenous seat: this is a genuine rope (coordination frame enabling shared decision-making). From the settler-state seat (especially resource extraction and development constituencies): this is a snare (constrained unilateral authority, suppressed extraction options, organized resistance requiring suppression). From the international arbitration seat: this is a tangled_rope (coordination mechanism binding both parties, but asymmetric in that one party — settler state — has lost unilateral authority). The engine computes per-seat types from the structural data. The authored claim (tangled_rope) reflects the international-law-reading seat; divergence from computed types at other seats is exactly the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations: beneficiary + low directionality. They gain decision-making authority and veto rights; they are constrained by the requirement to organize and negotiate continuously (mobile→constrained shift), but the net effect is a shift toward their sovereignty claims. Settler state: target + high directionality. It loses unilateral extraction authority; directionality near 1.0 (full target). Resource extraction industries: target + high directionality (0.85+). Exit options are constrained — they can litigate the treaty framework or arbitrage to other territories, but core markets are barred by consent requirements. International arbitration bodies: observer + analytical directionality. They interpret and enforce, but collect no rents; the constraint serves their institutional interest in treaty law authority, but they are not extractive beneficiaries. Third-party claimants: excluded + constrained. They are structurally barred from the negotiation surface, an intentional exclusion, not a mobility constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids false mandatrophy detection because the founding problem is live (unilateral settler-state treaty reinterpretation persists as a live practice; international courts continue to see cases where states claim treaties are superseded). The constraint's mandatrophy would only trigger if (a) unilateral state authority were genuinely accepted by all parties as supreme, OR (b) the founding problem (state override of treaties) were demonstrably extinct. Neither is true. The measurement showing theater-ratio increase (0.42→0.58) could trigger a piton hypothesis if extraction were also collapsing; instead, extraction stabilizes at moderate-high levels, indicating the constraint is performing both coordination and enforcement functions rather than purely theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settler_state_legislative_override_capacity,
    'Can a settler-state legislature unilaterally override or reinterpret a treaty that this reading treats as binding international law, or is the treaty supreme over later legislation?',
    'Constitutional court rulings on treaty supremacy, or clarifying legislation establishing hierarchy of treaty law vs. domestic statute. Empirical test: does a settler state''s high court enforce treaties against contradictory legislation?',
    'If treaties are supreme, the nation-to-nation reading is structurally stable (settler state constrained by prior commitment). If legislature can override, the reading''s constraint degrades to performative — Indigenous nations retain nominal veto but face legislative override, shifting the classification toward snare (suppression dominates coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(settler_state_legislative_override_capacity, empirical, 'Whether treaty law is supreme over settler-state legislation or subordinate to legislative override.').

omega_variable(
    international_arbitration_enforceability,
    'Do international arbitration bodies have effective enforcement mechanisms (sanctions, trade penalties, reparations compliance) for settler-state treaty violations, or is arbitration declaratory only?',
    'Track enforcement rates: do settlers states comply with arbitration rulings on Indigenous rights? Historical analysis of post-judgment settler-state compliance with international court orders on territorial or resource matters.',
    'If enforcement is effective, the constraint is stable and genuinely constraining (tangled_rope with real asymmetry). If declaratory only, the constraint degrades to theater — Indigenous nations have a formal veto and arbitration backing, but no mechanism to block resource extraction if the settler state defies arbitration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_arbitration_enforceability, empirical, 'Whether international arbitration enforcement is substantive or performative.').

omega_variable(
    consent_fragmentation_and_organized_opposition,
    'Can Indigenous nations maintain unified consent positions, or are they structurally fragmented by internal divisions (generational, environmental, economic), allowing settler-state arbitrage through selective engagement?',
    'Empirical study of consent negotiations: do settler states succeed in dividing Indigenous nations by offering side deals to factions, or do consensus mechanisms hold? Analysis of specific resource negotiations (pipeline projects, mining, timber) where consent was withheld vs. fractured.',
    'If fragmentation dominates, the nation-to-nation frame is undermined — settler states can maintain effective unilateral authority by playing factions against each other, converting the constraint into a snare (consent formalism with no real veto). If consensus mechanisms hold, the constraint remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_fragmentation_and_organized_opposition, empirical, 'Whether Indigenous nations can maintain unified consent positions or face structural fragmentation enabling settler-state arbitrage.').

omega_variable(
    reading_foreclosure_via_axiom_contradiction,
    'Does this reading''s core axiom (indigenous_nations_hold_ongoing_sovereignty) logically foreclose the extinguishment reading''s core axiom (indigenous_territorial_sovereignty_was_ceded), or do they coexist in separate legal frameworks?',
    'Jurisprudential analysis: can a single settler state''s legal system coherently enforce both readings simultaneously (e.g., treating some treaties as extinguished and others as nation-to-nation)? Court decisions that address both readings show whether they are foreclosed or context-dependent.',
    'If foreclosed (logically incompatible), the readings are in zero-sum competition and only one can prevail. If coexisting (context-dependent), the constraint landscape is hybrid — some territories under nation-to-nation framing, others under extinguishment, generating persistent legal instability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_via_axiom_contradiction, conceptual, 'Whether the nation-to-nation and extinguishment readings are logically foreclosed from each other or can coexist in legal pluralism.').

omega_variable(
    performance_vs_substance_in_consultation,
    'How much of the measured theater_ratio (0.58) represents genuine consent-sharing vs. performative consultation (listen-and-proceed procedures, token representation, consultation without veto)?',
    'Process audit: track consultation outcomes over a 5-year period; measure what fraction of Indigenous objections result in project modification or blocking vs. proceeding despite objection. Compare stated consent requirements to actual decision outcomes.',
    'High performance ratio (theater is mostly procedural compliance without substantive veto) indicates the constraint is already degraded toward snare (suppression masks coordination failure). Low performance ratio (consultation has real effect on outcomes) indicates the constraint is functioning as tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_vs_substance_in_consultation, empirical, 'Whether consultation procedures translate into substantive veto authority or remain performative without outcome control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hist_tr_t5, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 5, 0.46).
narrative_ontology:measurement(hist_tr_t10, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 10, 0.51).
narrative_ontology:measurement(hist_tr_t15, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(hist_tr_t20, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 20, 0.57).
narrative_ontology:measurement(hist_tr_t25, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement(hist_tr_t30, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement(hist_tr_t40, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(hist_be_t5, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(hist_be_t10, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(hist_be_t15, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(hist_be_t20, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(hist_be_t25, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(hist_be_t30, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(hist_be_t40, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hist_su_t5, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(hist_su_t10, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(hist_su_t15, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(hist_su_t20, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(hist_su_t25, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(hist_su_t30, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(hist_su_t40, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__nation_to_nation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(historical_treaty_substrate__nation_to_nation_reading, 0.18).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the historical_treaty_substrate kernel. The extinguishment reading treats treaties as completed transactions; this reading treats them as ongoing sovereign-to-sovereign agreements. The stewardship reading treats them as relational coexistence pacts. All three share the same historical kernel — the persisting commitment to treaties — but instantiate different constraints through different readings of what treaties ARE and what they obligate. They are linked as siblings in the constraint family, not as hierarchical versions. Each story carries its own ε, beneficiary/victim structure, and type classification. The network edges indicate mutual influence: this nation-to-nation reading influences the stewardship reading by establishing international law authority; it forecloses the extinguishment reading by denying that sovereignty was ceded.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historical_treaty_substrate__nation_to_nation_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
