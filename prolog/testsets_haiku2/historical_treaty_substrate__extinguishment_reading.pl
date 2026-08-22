% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__extinguishment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__extinguishment_reading, []).

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
 *   constraint_id: historical_treaty_substrate__extinguishment_reading
 *   human_readable: Treaty Extinguishment: Sovereignty Cession as Completed Property Transaction
 *   domain: legal/constitutional/indigenous_law
 *
 * SUMMARY:
 *   The extinguishment reading interprets historical treaties signed between
 *   settler states and Indigenous nations as completed property transactions.
 *   Under this reading, Indigenous nations ceded full territorial sovereignty
 *   in exchange for defined reserves and annuities. The settler state thereby
 *   became the exclusive legitimate authority over ceded territory, with
 *   Indigenous nations retaining governance authority only over reserves and
 *   a narrow set of treaty-protected rights. This reading is one of three
 *   contested interpretations of the same historical kernel (the ambiguous
 *   treaty texts and their meaning). The other readings—nation-to-nation and
 *   stewardship—assign different structural consequences and different
 *   victim/beneficiary sets. The extinguishment reading is the only one that
 *   treats Indigenous sovereignty as fully transferred and settler-state
 *   authority as exclusive. It produces high extraction (0.82) because the
 *   transfer is asymmetric and its persistence requires continuous
 *   suppression of alternative interpretations. The founding problem
 *   (boundary ambiguity in historical treaties) is characterized as 'dead' by
 *   the settler state but 'living' by Indigenous nations and human rights
 *   interpreters—a mismatch that signals zombie constraint dynamics.
 *
 * KEY AGENTS:
 *   - settler_state_authority: institutional agenda-setter, maintains exclusive interpretive authority over treaty meaning, benefits from the reading
 *   - indigenous_nations_post_cession: organized payer and secondary beneficiary, identity-locked victims of the reading, cannot exit without renouncing collective identity
 *   - settler_commercial_interests: powerful beneficiary, depend on the reading for legal certainty of land title and resource rights
 *   - settler_courts_and_legislatures: institutional agenda-setter, interpret and enforce the reading through doctrine and statute
 *   - international_human_rights_bodies: excluded observer, would contest the reading but are structurally barred from interpretive authority
 *   - treaty_signatories_indigenous_leadership: historical observer, their intentions are reinterpreted through the extinguishment frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, 0.82).
domain_priors:suppression_score(historical_treaty_substrate__extinguishment_reading, 0.77).
domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Treaty Extinguishment: Sovereignty Cession as Completed Property Transaction").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal/constitutional/indigenous_law").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, '77317dc9-8309-462f-ad23-ce661645e20c').
narrative_ontology:cs_kernel_codification('77317dc9-8309-462f-ad23-ce661645e20c', fixed_text).
narrative_ontology:cs_authority_grounding('77317dc9-8309-462f-ad23-ce661645e20c', extraction).
narrative_ontology:cs_interpretation_layer_present('77317dc9-8309-462f-ad23-ce661645e20c').
narrative_ontology:cs_reading_relation('77317dc9-8309-462f-ad23-ce661645e20c', historical_treaty_substrate__nation_to_nation_reading, forecloses).
narrative_ontology:cs_reading_relation('77317dc9-8309-462f-ad23-ce661645e20c', historical_treaty_substrate__stewardship_reading, forecloses).
narrative_ontology:cs_axiom('77317dc9-8309-462f-ad23-ce661645e20c', foundational, full_sovereignty_transfer_completed).
narrative_ontology:cs_axiom_status(full_sovereignty_transfer_completed, holdable).
narrative_ontology:cs_axiom_grounding('77317dc9-8309-462f-ad23-ce661645e20c', full_sovereignty_transfer_completed, empirically_contingent).
narrative_ontology:cs_axiom('77317dc9-8309-462f-ad23-ce661645e20c', foundational, settler_state_exclusive_authority_inherent).
narrative_ontology:cs_axiom_status(settler_state_exclusive_authority_inherent, holdable).
narrative_ontology:cs_axiom_grounding('77317dc9-8309-462f-ad23-ce661645e20c', settler_state_exclusive_authority_inherent, conventional).
narrative_ontology:cs_reference_frame('77317dc9-8309-462f-ad23-ce661645e20c', complete_sovereignty_cession).
narrative_ontology:cs_drift_state('77317dc9-8309-462f-ad23-ce661645e20c', contemporary_human_rights_challenge_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('77317dc9-8309-462f-ad23-ce661645e20c', '2026-06-15T14:32:00Z').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_state_authority).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_commercial_interests).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_nations_post_cession).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, indigenous_nations_post_cession).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, non_indigenous_settler_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes the legal and administrative infrastructure that enforces the extinguishment reading. Interprets historical treaties as concluded property transactions that transferred full territorial sovereignty from Indigenous nations to the settler state in exchange for reserve lands and annuities. Controls the interpretive authority through courts, legislatures, and administrative agencies. Collects benefit through exclusive authority to allocate ceded territory (mineral rights, development, governance). Maintains the reading by rejecting alternative interpretations and suppressing Indigenous claims to coexisting jurisdiction over ceded lands.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_state_authority, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Ceded territorial sovereignty in historical treaties, receiving in exchange defined reserve lands (typically a fraction of the ceded territory) and annuities (often nominal, historically unpaid or withheld). Under the extinguishment reading, they retain governance authority only over reserve lands and a narrow set of treaty rights (hunting, fishing, cultural practice), exercisable only on specified lands or subject to settler-state regulation. Their territorial identity—historically rooted in the ceded lands—is severed; they become resident groups on alienated reserves rather than sovereign nations over inherited territory. Exit from this arrangement means renouncing collective indigenous identity and territorial connection, which is identity-locked for most Indigenous actors. Resistance is high but constrained by institutional asymmetry and internalized suppression from centuries of enforcement.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_nations_post_cession, payer,
    organized, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, indigenous_nations_post_cession, beneficiary).

% Acquire exclusive commercial access to ceded territory (timber, minerals, water, agricultural land, development) under the extinguishment reading. The reading provides the legal certainty they depend on: Indigenous claims to territorial sovereignty or coexisting jurisdiction would create ambiguity about land title and extractive rights. They benefit from the constraint's enforcement machinery, which excludes Indigenous authority from ceded lands. Can exit if alternative interpretations of treaties provided equivalent clarity (low switching cost to a different reading), but depend on active suppression of nation-to-nation and stewardship readings to maintain their position.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_commercial_interests, beneficiary,
    powerful, generational, mobile, national).

% Historical actors (now deceased) who negotiated and signed treaties. Under the extinguishment reading, they are characterized as having voluntarily ceded sovereignty in a binding property transaction. This characterization is contested: they may have understood treaties as relational pacts or nation-to-nation agreements with continuing obligations rather than completed property sales. The reading freezes their intentions into the extinguishment frame and bars renegotiation. Modern Indigenous nations cannot exit or renegotiate the treaties their ancestors signed because the reading treats the transaction as finalized and complete.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, treaty_signatories_indigenous_leadership, observer,
    powerful, biographical, trapped, local).

% Interpret and enforce the extinguishment reading through litigation, constitutional doctrine, and statutory framework. Courts treat historical treaties as completed property transfers and reject Indigenous claims for coexisting jurisdiction or territory restoration. Legislatures enact statutes that assume the extinguishment reading is settled law. They maintain the reading by interpreting ambiguous treaty language in favor of extinguishment and by creating procedural barriers to reopening settled historical claims.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_courts_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Would object to the extinguishment reading on grounds that unilateral reinterpretation of historical treaties violates contemporary human rights norms (UNDRIP, ILO 169, ICESCR) requiring Indigenous consent and recognition of collective rights. They are excluded from the interpretive authority that enforces the reading—settler-state courts do not treat international human rights instruments as binding reinterpretation keys for historical treaties. Their objection exists but is structurally suppressed by the settler-state's assertion of sovereign interpretive authority.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, international_human_rights_bodies, excluded,
    institutional, generational, constrained, global).

% Benefit from the extinguishment reading through stable land title, access to public lands and resources, and absence of negotiation requirements with Indigenous nations over land use. The reading provides legal certainty that the settler state has exclusive authority to allocate territory, which supports property markets, resource extraction, and development planning. They do not directly enforce the reading but depend on its enforcement for their property security and economic opportunities.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, non_indigenous_settler_populations, beneficiary,
    moderate, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__extinguishment_reading, settler_state_authority).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__extinguishment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves a boundary-setting problem: establishes which party (settler state or Indigenous nations) holds exclusive authority over ceded territory and its resources. The extinguishment reading solves this by assigning exclusive authority to the settler state, creating legal clarity for property rights, development, and resource allocation. This was the explicit functional purpose: to move from contested territorial claim to settled jurisdictional hierarchy.
% TRANSFER_FUNCTION: Moves territorial sovereignty from Indigenous nations to the settler state. Indigenous nations receive reserve lands (typically 1–5% of ceded territory) and annuities (often nominal or unpaid). The settler state and settler commercial interests receive exclusive authority over ceded territory (99–95% of the original Indigenous estate) and all commercial value (minerals, timber, water, development rights). The transfer is asymmetric: what Indigenous nations surrender (territorial identity, jurisdiction, resource base) vastly exceeds what they receive (confined reserves, modest annuities).
% ABSENT_VOICES: International human rights interpreters are structurally excluded: they would argue the reading violates modern norms of Indigenous self-determination and consent, and that treaties must be reinterpreted in light of contemporary human rights standards. Indigenous nations whose treaty interpretation differs from extinguishment are also excluded from the settler-state's interpretive authority—they are heard in litigation but their alternative readings are systematically rejected as non-binding or historically baseless.
% DISAPPEARANCE_RATIONALE: If the extinguishment reading vanished overnight and one of the alternative readings (nation-to-nation or stewardship) replaced it, Indigenous nations would have standing to reassert coexisting jurisdiction over ceded territories, demand renegotiation of treaties, and claim restoration of lands or revenue shares. Settler commercial interests would lose legal certainty for their land titles and resource rights. Property markets would destabilize; resource extraction projects would face jurisdictional disputes. The settler state would lose exclusive authority and would need to negotiate with Indigenous nations as coequal sovereigns rather than administer ceded lands unilaterally. The entire structure of settler-state land allocation, property law, and resource governance depends on the extinguishment reading.
% FOUNDING_PROBLEM: Historical treaties created ambiguity about the extent of Indigenous cession. The treaties used language like 'cede, release, and surrender' but also recognized Indigenous 'hunting rights,' 'reserves,' and ongoing relationships. The settler state needed to resolve this ambiguity in its favor to enable unconstrained development, resource extraction, and settlement. The extinguishment reading was the legal technology for transforming ambiguous historical language into unambiguous surrender of Indigenous sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The settler state's courts and legislatures attest the founding problem (boundary ambiguity) is solved by the extinguishment reading. Indigenous nations, international human rights bodies, and independent legal historians attest the founding problem is NOT solved—it is obscured. They argue that historical treaties remain genuinely ambiguous and that the settler state unilaterally imposed the extinguishment reading to resolve ambiguity in its own favor, not to discover the parties' original intent. The problem is thus contested: the beneficiaries claim it is solved, the victims and external observers claim it has been suppressed rather than resolved.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__extinguishment_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__extinguishment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__extinguishment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(historical_treaty_substrate__extinguishment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__extinguishment_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__extinguishment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__extinguishment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extinguishment reading is claimed as tangled_rope because it coordinates a boundary-setting function (resolves ambiguity about who has territorial authority) while extracting asymmetrically (Indigenous nations receive narrow reserves and nominal annuities in exchange for sovereignty and territory). The metrics reflect this hybrid: extractiveness is high (0.82) because the transfer vastly favors the settler state; suppression is high (0.77) because the reading's persistence depends on actively excluding alternative interpretations and suppressing Indigenous claims to coexisting jurisdiction; theater is moderate-high (0.58) because settler-state authorities frame the reading as neutral historical law discovery ('what the treaties actually meant') while it operates as selective interpretation that happens to benefit settler interests. The temporal trajectory shows rising extractiveness (0.64→0.82) and rising theater (0.42→0.58) across 266 years, indicating that the reading's extractive character has accumulated and its performance (framing as neutral law) has intensified as Indigenous nations have mounted increasing resistance. The suppression trajectory plateaus after 1980, suggesting enforcement machinery has matured and no longer needs to intensify—the reading is now institutionally embedded. Accessibility_collapse is moderate (0.71) because while alternatives theoretically exist, they are systematically excluded from mainstream settler-state legal authority, creating practical foreclosure while maintaining formal openness to debate.
 *
 * PERSPECTIVAL GAP:
 *   The settler-state agenda-setter seat (courts, legislatures, commercial interests) experiences this constraint as genuine boundary-resolution that was settled 150+ years ago and is now background law. The Indigenous nations seat experiences this as active, ongoing extraction masked by historical framing. The gap is not about disagreement on facts but on frames: the settler-state frame treats the reading as a discovery of what historical treaties meant; the Indigenous frame treats it as a unilateral imposition of one interpretation among contested alternatives. This is a frame-level divergence, not a metric divergence. The engine computes per-seat types from the structural data (beneficiary/victim, power, exit, directionality); this frame-level gap is routed to omegas to document the interpretive contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Settler-state authority and settler commercial interests are structural beneficiaries: they set the interpretive terms (d near 0.0 for beneficiary), they control the enforcement machinery, they receive territorial sovereignty and exclusive commercial access. Indigenous nations are structural targets: they surrendered territory, receive confined reserves, cannot exit without renouncing identity (d near 1.0 for target). The asymmetry is sharpened by exit options: the settler state can shift to alternative readings (mobile, arbitrage-grade) without institutional cost; Indigenous nations cannot exit without identity dissolution (identity_locked). This directionality cascade produces high effective extraction (χ) for the victim seat and low or negative χ for the beneficiary seat. International human rights bodies and alternative interpretation communities are excluded from the settler-state's interpretive authority, which suppresses their ability to apply alternative directionality logics to the same historical material.
 *
 * MANDATROPHY ANALYSIS:
 *   The extinguishment reading exhibits classic mandatrophy symptoms: the founding problem (treaty ambiguity) is declared dead and solved by the settler state, but Indigenous nations, human rights interpreters, and legal historians continue to treat it as live and unsolved. This mismatch (founding_problem_status=dead from settler state, =live from Indigenous/external observers) is the diagnostic signal of mandatrophy. The constraint persists because the settler state maintains interpretive monopoly and enforcement machinery, not because the founding problem is genuinely solved. The theater_ratio rise (0.42→0.58) indicates that an increasing share of settler-state enforcement activity is devoted to maintaining the reading's appearance of neutrality and inevitability, rather than functional boundary-resolution. If the founding problem is truly solved, why must settlement denial efforts intensify? The temporal pattern suggests the reading is approaching piton dynamics—it persists by institutional inertia and performative affirmation rather than by genuine coordination benefit. However, it remains tangled_rope rather than piton because the settler state and commercial interests still actively profit from the reading's enforcement; the beneficiary has not attenuated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_language_ambiguity,
    'What did the historical treaty signatories actually intend by language like ''cede, release, and surrender''? Did they intend to transfer full territorial sovereignty, or to recognize bounded cession of jurisdiction over specific lands while retaining relational coexistence?',
    'Linguistic and historical analysis of treaty texts with Indigenous nation oral histories, contemporary documents from both parties, and comparative analysis of other colonial treaties. Oral history testimony from Indigenous descendants of original signatories. Ethnographic and linguistic study of how key terms were translated between European languages and Indigenous languages at the time.',
    'If the evidence shows signatories intended relational coexistence (nation-to-nation or stewardship readings), the extinguishment reading is a post-hoc reinterpretation, not a discovery of original intent. This would support reclassification to snare and grounds for reopening treaties. If the evidence shows full-sovereignty transfer was intended, the extinguishment reading is historically grounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_language_ambiguity, empirical, 'Historical intent ambiguity in treaty language and its interpretation.').

omega_variable(
    reading_enforcement_dynamics,
    'Is the extinguishment reading''s persistence driven by its genuine explanatory power and coordination function, or by the settler state''s institutional power to impose and maintain it through suppression of alternatives?',
    'Comparative jurisdictional analysis: jurisdictions that have adopted nation-to-nation or stewardship readings (some Canadian provinces, some international arbitration contexts) should show functionally equivalent coordination outcomes with different distributional consequences. If coordination succeeds without extinguishment, then extinguishment is about extraction, not coordination.',
    'If alternatives produce equivalent coordination, the extinguishment reading is extractive cover for institutional power, supporting reclassification toward snare. If alternatives degrade coordination (create ambiguity, legal instability), extinguishment is genuinely coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_enforcement_dynamics, empirical, 'Whether the reading''s persistence is functional or coercive.').

omega_variable(
    identity_lock_internalization_trajectory,
    'Is Indigenous suppression of alternative interpretations structural (settler-state legal barriers, resource barriers, institutional exclusion) or internalized (Indigenous nations have absorbed the extinguishment reading as inevitable or legitimate)?',
    'Post-institutional-shift analysis: Indigenous nations in jurisdictions that have formally recognized nation-to-nation or stewardship readings (some Canadian contexts) show whether they reassert territorial claims and alternative readings, or continue suppressed even when barriers are lowered. Internalization persists after barrier removal; structural suppression is lifted.',
    'If suppression is primarily internalized, reclassification requires addressing internalized legitimacy (deeper identity work, counter-narratives). If structural, reclassification can happen through institutional change alone. Mixed findings would indicate the suppression operates at both levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization_trajectory, empirical, 'Structural vs. internalized suppression mechanism for Indigenous resistance to the reading.').

omega_variable(
    kernel_reading_foreclosure,
    'Are the nation-to-nation and stewardship readings logically foreclosed by the extinguishment reading''s core premise, or do they coexist as genuinely live options in alternative frameworks?',
    'Logical-philosophical analysis of axioms in each reading: do they directly contradict within a single framework, or do they describe genuinely different commitments that could hold in different parties'' frameworks? International law principle testing: does recognizing Indigenous sovereignty-retention (nation-to-nation) logically foreclose settler-state exclusive authority (extinguishment), or do they describe coexisting but asymmetrical authority structures?',
    'If foreclosed (truly contradictory), the extinguishment reading is the only consistent option and the constraint is closer to natural law. If coexisting (compatible but incommensurate), all three readings remain live and the extinguishment reading''s dominance is entirely institutional, not logical—supporting reclassification toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical foreclosure vs. coexistence of sibling readings.').

omega_variable(
    remedy_accessibility_post_recognition,
    'If Indigenous nations'' alternative readings are formally recognized (nation-to-nation reading adopted), what remedies become available? Territory restoration, revenue sharing, renegotiation, coexisting jurisdiction? And are these remedies actually implemented, or recognized-in-principle but structurally blocked?',
    'Analysis of jurisdictions that have formally adopted nation-to-nation principles (Canada post-1982; some Latin American constitutions). Map the gap between formal recognition and actual remedy implementation. Track whether Indigenous nations gain substantive authority or merely consultative status.',
    'If remedies are implemented, the alternative readings have real power and the extinguishment reading''s dominance is purely institutional. If remedies are recognized but not implemented, alternative readings are performatively acknowledged but substantively suppressed—indicating theater_ratio remains high even after formal recognition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_accessibility_post_recognition, empirical, 'Implementation gap between formal recognition of alternative readings and actual remedy provision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 1760, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1760, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1760, 0.42).
narrative_ontology:measurement_basis(hist_tr_t1760, projected).
narrative_ontology:measurement(hist_tr_t1850, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1850, 0.48).
narrative_ontology:measurement_basis(hist_tr_t1850, observed).
narrative_ontology:measurement(hist_tr_t1920, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1920, 0.52).
narrative_ontology:measurement_basis(hist_tr_t1920, observed).
narrative_ontology:measurement(hist_tr_t1980, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1980, 0.55).
narrative_ontology:measurement_basis(hist_tr_t1980, observed).
narrative_ontology:measurement(hist_tr_t2010, historical_treaty_substrate__extinguishment_reading, theater_ratio, 2010, 0.57).
narrative_ontology:measurement_basis(hist_tr_t2010, observed).
narrative_ontology:measurement(hist_tr_t2026, historical_treaty_substrate__extinguishment_reading, theater_ratio, 2026, 0.58).
narrative_ontology:measurement_basis(hist_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(hist_be_t1760, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1760, 0.64).
narrative_ontology:measurement_basis(hist_be_t1760, projected).
narrative_ontology:measurement(hist_be_t1850, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1850, 0.71).
narrative_ontology:measurement_basis(hist_be_t1850, observed).
narrative_ontology:measurement(hist_be_t1920, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1920, 0.76).
narrative_ontology:measurement_basis(hist_be_t1920, observed).
narrative_ontology:measurement(hist_be_t1980, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1980, 0.79).
narrative_ontology:measurement_basis(hist_be_t1980, observed).
narrative_ontology:measurement(hist_be_t2010, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 2010, 0.81).
narrative_ontology:measurement_basis(hist_be_t2010, observed).
narrative_ontology:measurement(hist_be_t2026, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 2026, 0.82).
narrative_ontology:measurement_basis(hist_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1760, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1760, 0.68).
narrative_ontology:measurement_basis(hist_su_t1760, observed).
narrative_ontology:measurement(hist_su_t1850, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1850, 0.71).
narrative_ontology:measurement_basis(hist_su_t1850, observed).
narrative_ontology:measurement(hist_su_t1920, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1920, 0.74).
narrative_ontology:measurement_basis(hist_su_t1920, observed).
narrative_ontology:measurement(hist_su_t1980, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1980, 0.76).
narrative_ontology:measurement_basis(hist_su_t1980, observed).
narrative_ontology:measurement(hist_su_t2010, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement_basis(hist_su_t2010, observed).
narrative_ontology:measurement(hist_su_t2026, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 2026, 0.77).
narrative_ontology:measurement_basis(hist_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__extinguishment_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(historical_treaty_substrate__extinguishment_reading, 0.12).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate__nation_to_nation_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate__stewardship_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories share the historical_treaty_substrate kernel. They differ in their interpretation of what historical treaties transferred and what authority Indigenous nations retain. The extinguishment_reading treats the cession as complete and settled; nation_to_nation_reading treats treaties as ongoing sovereign-equal agreements subject to modern principles; stewardship_reading treats treaties as relational coexistence pacts with no sovereignty transfer. Each reading has its own ε (extinguishment ε=0.82 as extractive imposition, nation-to-nation ε lower as it recognizes Indigenous authority, stewardship ε lower as it denies cession). The three readings form a constraint family linked by kernel contest. The sibling readings' classification depends partly on this reading's acceptance or rejection—if extinguishment is the law, alternatives are foreclosed; if alternatives are adopted, extinguishment is reclassified as overridden doctrine. Each story is self-standing (one reading, one ε, one type); the family relationship is recorded in network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historical_treaty_substrate__extinguishment_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
