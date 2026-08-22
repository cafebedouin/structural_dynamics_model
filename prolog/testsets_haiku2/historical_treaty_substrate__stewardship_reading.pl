% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__stewardship_reading, []).

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
 *   constraint_id: historical_treaty_substrate__stewardship_reading
 *   human_readable: Historical Treaty Substrate—Stewardship Reading
 *   domain: legal_anthropology/indigenous_law
 *
 * SUMMARY:
 *   Historical treaties between Indigenous nations and settler states are
 *   read by the stewardship interpretation as relational pacts establishing
 *   covenantal obligation—shared territorial stewardship with no cession of
 *   Indigenous sovereignty. This reading instantiates one constraint within
 *   the contested kernel 'historical_treaty_substrate.' The kernel
 *   encompasses three structurally distinct readings: (1)
 *   extinguishment_reading—treaties as completed property transactions
 *   severing Indigenous territorial claims; (2)
 *   nation_to_nation_reading—treaties as international agreements between
 *   sovereign equals, subject to modern treaty law; (3) stewardship_reading
 *   (this constraint)—treaties as living covenants for durable coexistence.
 *   Each reading describes a different constraint because each carries
 *   different ε (extractiveness), different beneficiary/victim sets, and
 *   different classification. The stewardship reading authors the
 *   constraint's ε as the extractiveness of the standing arrangement under
 *   contest—the treaty framework as interpreted by Indigenous nations, courts
 *   recognizing Indigenous consent authority, and co-management regimes. The
 *   ε referent is NOT the alternative readings' endorsed arrangements (those
 *   would make ε≈0 for Indigenous advocacy readings); it is the actual
 *   historical treaty substrate interpreted through the stewardship lens.
 *
 * KEY AGENTS:
 *   - Indigenous nations: beneficiaries of territorial jurisdiction and consent authority; identity-locked to the territorial relationship; organized power
 *   - Settler-state resource extraction apparatus: payer (constrained by consent requirements); institutional power; bears negotiation costs and foregone unilateral extraction
 *   - Settler courts and commissions: agenda-setter (interpreting treaties); institutional power; constrained exit (cannot ignore treaties without repudiating legitimacy)
 *   - Settler nation public: dual-positioned (beneficiary of resource access and stability, payer of co-management costs); moderate power; mobile exit
 *   - Treaty scholars and interpreters: observers (shape court interpretation); moderate power; analytical exit
 *   - Competing settler nations: excluded (would access territories if treaties were weakened); institutional power; trapped by the treaty framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, 0.31).
domain_priors:suppression_score(historical_treaty_substrate__stewardship_reading, 0.58).
domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__stewardship_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__stewardship_reading, "Historical Treaty Substrate—Stewardship Reading").
narrative_ontology:topic_domain(historical_treaty_substrate__stewardship_reading, "legal_anthropology/indigenous_law").

domain_priors:requires_active_enforcement(historical_treaty_substrate__stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__stewardship_reading, '42901fc0-ad1c-482e-8c88-881ded964e19').
narrative_ontology:cs_kernel_codification('42901fc0-ad1c-482e-8c88-881ded964e19', fixed_text).
narrative_ontology:cs_authority_grounding('42901fc0-ad1c-482e-8c88-881ded964e19', lineage).
narrative_ontology:cs_interpretation_layer_present('42901fc0-ad1c-482e-8c88-881ded964e19').
narrative_ontology:cs_reading_relation('42901fc0-ad1c-482e-8c88-881ded964e19', historical_treaty_substrate__extinguishment_reading, coexists_with).
narrative_ontology:cs_reading_relation('42901fc0-ad1c-482e-8c88-881ded964e19', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('42901fc0-ad1c-482e-8c88-881ded964e19', foundational, indigenous_territorial_sovereignty_retained).
narrative_ontology:cs_axiom_status(indigenous_territorial_sovereignty_retained, holdable).
narrative_ontology:cs_axiom_grounding('42901fc0-ad1c-482e-8c88-881ded964e19', indigenous_territorial_sovereignty_retained, deontological).
narrative_ontology:cs_axiom('42901fc0-ad1c-482e-8c88-881ded964e19', foundational, ongoing_consent_requirement_enforceable).
narrative_ontology:cs_axiom_status(ongoing_consent_requirement_enforceable, holdable).
narrative_ontology:cs_axiom_grounding('42901fc0-ad1c-482e-8c88-881ded964e19', ongoing_consent_requirement_enforceable, conventional).
narrative_ontology:cs_reference_frame('42901fc0-ad1c-482e-8c88-881ded964e19', covenantal_stewardship_obligation).
narrative_ontology:cs_drift_state('42901fc0-ad1c-482e-8c88-881ded964e19', contemporary_extraction_acceleration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('42901fc0-ad1c-482e-8c88-881ded964e19', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, territorial_ecosystem).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, settler_state_resource_extraction_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_nation_public).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, settler_nation_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold ancestral territorial jurisdiction and covenantal obligation to steward lands for future generations. Under this reading, treaties secure ongoing consent authority and deny unilateral alienation of territory. Their practical leverage is cultural continuity, inter-nation alliance, and legal redress through courts and commissions recognizing treaty rights. Exit is identity-fused: leaving the treaty claim means abandoning the territorial relationship that constitutes them as nations.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, indigenous_nations, beneficiary,
    organized, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, indigenous_nations, agenda_setter).

% Operates the legal and commercial machinery for resource extraction, agriculture, urban development, and administrative control over territories. Under the stewardship reading, this apparatus is constrained by ongoing Indigenous consent requirements and must negotiate joint management of resources rather than claim unilateral dominion. Its costs are negotiation overhead, operational delays, and foregone unilateral revenue. Its leverage is de facto state control and legislative authority, which it can exercise or relinquish through treaty recognition.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state_resource_extraction_apparatus, payer,
    institutional, generational, constrained, national).

% Adjudicate the treaty's interpretation and enforce its terms against both parties. They are the institutional arbiter of whether treaties are completed transactions (extinguishment reading) or living covenants (stewardship reading). Their role under this reading is to construe treaties generously in Indigenous favor, recognize evolving Indigenous consent mechanisms, and enforce co-management arrangements. Exit is constrained: they cannot ignore treaties without repudiating state legitimacy claims.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_courts_and_commissions, agenda_setter,
    institutional, generational, constrained, national).

% Enjoys access to lands, resources, and infrastructure built on contested territories. They also incur costs where treaty obligations redirect resources to co-management, Indigenous nations, or ecosystem restoration. Their stake is diffuse: they have no organized participation in treaty governance but benefit from political stability that secure treaties provide, and pay where treaty obligations limit resource access or raise commodity costs.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_nation_public, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, settler_nation_public, payer).

% Scholars, translators, legal commentators who interpret treaty texts and their history. They hold no enforcement authority but shape what courts accept as the authoritative reading. Under the stewardship reading, they emphasize Indigenous language versions, oral covenants predating written documents, continuity language ('as long as the sun rises'), and the relational framework embedded in many treaty preambles.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, treaty_scholars_and_interpreters, observer,
    moderate, generational, analytical, national).

% Would benefit from access to the same territories if treaty exclusivity were weakened or extinguished. They are kept out by the same treaty framework; their absence from the negotiating table is structural to the arrangement. They cannot voice alternative readings without undermining the treaty system itself.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, competing_settler_nations, excluded,
    institutional, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__stewardship_reading, settler_state_resource_extraction_apparatus).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a durable framework for coexistence between Indigenous nations exercising territorial jurisdiction and settler states exercising de facto administrative control. Solves the coordination problem of how two sovereignties can occupy the same space without war: by replacing conquest/cession narratives with covenantal obligation—mutual non-abandonment, shared resource stewardship, and ongoing consent.
% TRANSFER_FUNCTION: Transfers territorial jurisdiction and governance authority from unilateral settler-state dominion to shared/divided stewardship between Indigenous nations and settler institutions. Moves recognition (settler acknowledgment of Indigenous nationhood and consent authority) from the settler state toward Indigenous nations. Moves resources from unilateral extraction toward negotiated co-management and Indigenous benefit-sharing.
% ABSENT_VOICES: Territorial ecosystems cannot articulate their own stewardship needs, but under this reading the treaty framework designates Indigenous nations as trustees speaking for land; settler environmental constituencies that would support Indigenous stewardship but are organizationally excluded from treaty governance; and future generations of both nations (represented in principle, rarely in practice). Voices actively excluded: resource extractors who would argue for unilateral state dominion, and competing settler nations who would access the same territories if treaties were weakened.
% DISAPPEARANCE_RATIONALE: If the stewardship reading framework vanished—if courts reverted to the extinguishment reading and Indigenous consent authority were erased—territorial governance would consolidate under settler-state dominion, resource extraction would accelerate without consent procedures, and Indigenous political claims would lose their legal purchase. The opposite rearrangement would also follow: if the stewardship reading were universally enforced with real veto authority, settler state resource agencies would have to restructure into co-management bodies with Indigenous veto rights, some resource extraction would cease pending genuine consent, and Indigenous nations would recover operational control over portions of their territories.
% FOUNDING_PROBLEM: Indigenous peoples and settler colonies on the same territories faced an irresolvable conflict: conquest generated claims of sovereignty erasure; outright extermination was incomplete; the alternative was a framework for durable coexistence that honored Indigenous territorial relationships while acknowledging settler settlement as an irreversible fact. Treaties were intended to solve this by establishing mutual obligation—Indigenous parties acknowledged settler presence; settlers acknowledged Indigenous jurisdiction and binding commitments to coexist.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous nations and scholars of Indigenous law attest the founding problem remains live: ongoing disputes over consent authority, resource extraction, and territorial control prove the coexistence framework was never fully implemented. Settler-state legal authorities and extractive industries attest the problem is solved (treaties are completed, territorial control is settled, the founding dispute is historical). The mismatch—a founding problem attested as live by one party and as dead by the other—is the core political fact the stewardship constraint encodes. Testimony from the settler_courts_and_commissions seat is mixed: some courts recognize the founding problem as live (increasing jurisprudence on Indigenous rights), others defer to legislative/executive closure of the dispute.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__stewardship_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(historical_treaty_substrate__stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__stewardship_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__stewardship_reading_tests).
:- end_tests(historical_treaty_substrate__stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.31 (interval end) because the stewardship reading interprets treaties as securing ongoing Indigenous consent authority without outright cession—extraction is present (settler states benefit from unilateral de facto control and resource access despite nominal consent requirements) but limited (consent requirements do create real operational friction and foregone extraction). Suppression at 0.58 has DECLINED over the interval (0.72→0.58) because legal recognition of Indigenous consent authority has grown (courts increasingly cite treaty protections, co-management agreements exist, consent mechanisms are institutionalized) while de facto suppression of Indigenous nations' voices has eroded. Theater ratio at 0.62 (interval end) is high and RISING (0.35→0.62) because the settler state increasingly stages treaty 'consultation' without real consent power-sharing, and courts perform treaty interpretation while limiting remedies; the constraint persists increasingly through theatrical maintenance rather than structural coordination. Accessibility collapse at 0.45 has DECLINED sharply (0.85→0.52 structural level) because alternatives to the treaty framework—direct Indigenous governance, territorial restitution, settler withdrawal—have become more organizationally and politically visible; the constraint no longer appears inevitable. Resistance at 0.72 (interval end) has RISEN dramatically (0.25→0.68 structural level) because Indigenous nations have built legal, political, and alliance capacity to contest the constraint's terms and demand real co-management. The coercion grid shows the differential effect: structural-level suppression has eroded while organizational and class-level resistance has grown; individual settler colonists still face high barriers to territorial exit, but Indigenous organizational capacity to contest the constraint has matured. Claim/metric independence: the constraint is CLAIMED as tangled_rope (genuine coordination function + asymmetric extraction requiring active enforcement) while the metrics describe substantially theatrical enforcement and declining suppression—a credible tangled_rope in decline, not an illusion.
 *
 * PERSPECTIVAL GAP:
 *   From the settler-state apparatus seat: the treaty is a completed historical document whose obligations are being fulfilled through consultation mechanisms and co-management boards; any extraction is justified by the coordination benefit and the irreversibility of settlement. From the Indigenous nations' seat: the treaty is a living covenant whose core promise—Indigenous consent authority and territorial jurisdiction—remains unenforced; extraction persists through theatrical compliance and legal doctrines that limit consent to narrow domains. From settler courts: the treaty language is ambiguous, requiring interpretation that balances settler governance needs against Indigenous rights; the stewardship reading is one plausible construal among others. The engine computes this divergence from the structural data—beneficiary/victim declarations and exit options—without requiring the author to adjudicate which seat's perception is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations hold d near the beneficiary end (0.2–0.3): they are the structured beneficiaries of territorial jurisdiction claims, though their exit is identity-locked (they cannot leave without ceasing to be themselves). The settler-state apparatus holds d near the target end (0.8–0.9): it bears the cost of consent requirements and foregone extraction, though it retains de facto institutional leverage. The settler public holds d symmetric (0.4–0.6): they benefit from access to resources and political stability that secure treaties provide, but also incur diffuse costs where treaties limit extraction or mandate co-management. Treaty interpreters are near analytical (d≈0.5): they have no direct stake but shape what courts accept. Competing settler nations are excluded (not in the d calculus at all). The directionality overrides for settler-court institutional actors would shift d toward the payer end (0.65–0.75) if the story established that courts are structurally captured by settler interests—the prompt provides no such evidence, so the derivation chain proceeds from beneficiary/victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The stewardship reading faces a classical mandatrophy signature: the founding problem (territorial coexistence between Indigenous and settler sovereignties) remains contested—Indigenous nations attest it is live (ongoing disputes over consent and extraction), while settler authorities attest it is solved (treaties are completed, consent is formalized, disputes are merely technical interpretation). This status divergence is NOT a classification error—it is the political fact the constraint encodes. The constraint persists through active maintenance (courts interpreting treaty language, governments staging consultations, Indigenous nations asserting claims) rather than by tacit acceptance. Theater ratio rising from 0.35 to 0.62 signals Goodhart drift: settler authorities increasingly perform treaty compliance (consultation rituals, co-management boards with limited decision power) without yielding the substantive consent authority the stewardship reading claims. A mandatrophy resolution would occur if Indigenous nations' legal and political capacity grew sufficiently that courts had to recognize real veto authority (true co-management rather than consultation theater), or if settler states formally abandoned the treaty framework and moved to unilateral resource extraction (extinguishment reading enforcement). The present state is stable mandatrophy: the constraint persists because the founding problem is genuinely undecided, and both parties have capacity to maintain their reading without destroying the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_text_temporality,
    'Is the treaty text a snapshot agreement capturing a moment of transfer (extinguishment reading), or is it a framework for an ongoing relationship (stewardship reading)?',
    'Textual analysis of treaty language comparing permanence markers (''forever,'' ''for as long as waters flow'') against transfer markers (''cede,'' ''surrender'') across English and Indigenous language versions; historical testimony from treaty negotiators and Indigenous oral accounts of negotiation intent.',
    'A textual reading emphasizing permanence and relationship language strengthens stewardship classification; emphasis on transfer language strengthens extinguishment. If oral accounts from Indigenous negotiators are recovered and clearly assert relationship/covenantal intent, the stewardship reading gains authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_text_temporality, empirical, 'Whether the treaty text encodes permanence and relationship or completed transfer.').

omega_variable(
    consent_authority_structure,
    'Does ''consent'' in the stewardship reading mean veto power over resource extraction and territorial use, or consultation without decision authority?',
    'Examination of actual co-management agreements and their enforcement: do Indigenous nations have veto authority over extraction projects, or only consultation rights? Do courts enforce Indigenous consent as binding or merely as a procedural step?',
    'If consent means veto authority, the stewardship reading produces a substantially different distribution of power (Indigenous nations as co-sovereigns); if consent means consultation, the constraint is mostly theatrical—theatrical enough to move toward piton classification. The measured theater_ratio rising to 0.62 suggests the latter, but the resolution mechanism distinguishes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_authority_structure, empirical, 'Whether Indigenous consent authority is substantive veto or procedural consultation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.58 at interval end) structural (legal doctrines limiting consent to narrow domains, courts deferring to settler state on extraction decisions) or internalized (Indigenous nations'' own legal consciousness incorporating settler property frameworks)?',
    'Post-assertion suppression trajectory: when Indigenous nations assert territorial jurisdiction or withhold consent outside settler-defined consultation channels, do settler authorities respond with force/legal sanction (structural), or do Indigenous negotiators self-censor and accept narrower claims (internalized)? Longitudinal study of community legal consciousness and assertion patterns.',
    'If suppression is structural, the constraint requires legal/political remedy (courts enforcing true consent authority). If partly internalized, the constraint persists because Indigenous nations'' own institutions have been shaped to accept limited consent authority; remedy requires identity-cultural work alongside legal change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression is external legal doctrine or internalized Indigenous constraint on their own claims.').

omega_variable(
    kernel_reading_foreclosure,
    'Do the extinguishment and stewardship readings genuinely coexist as live positions, or does the stewardship reading logically foreclose the extinguishment reading (i.e., can no single framework hold both)?',
    'Conceptual analysis: can a single constitutional framework assert both that ''Indigenous nations retain territorial jurisdiction per the stewardship covenant'' and ''Indigenous territorial sovereignty was extinguished by the treaty transfer''? Or are these logically inconsistent assertions about the same legal relationship? Examination of whether courts have attempted to hold both readings simultaneously and what logical strain results.',
    'If readings coexist_with (different parties hold them simultaneously in the same framework), the kernel remains undecided and mandatrophy persists. If stewardship forecloses extinguishment (they cannot coexist logically), the engine classifies the relationship as foreclosure and the framework either adopts stewardship or invokes a third option.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the stewardship and extinguishment readings are logically foreclosing or coexisting.').

omega_variable(
    settler_state_capture_of_interpretation,
    'Is the settler state''s control of courts and legislatures captured by settler interests (making treaty interpretation biased toward extinguishment/exploitation), or is there sufficient institutional autonomy for courts to adopt the stewardship reading on its structural merits?',
    'Comparative analysis of court decisions recognizing Indigenous rights and treaty obligations: are courts defending Indigenous consent authority against settler state agency resistance, or are courts merely formalizing settler state preferences? Do courts impose remedies (territorial restitution, veto authority) that settler agencies resist, or do they offer symbolic victories without material remedy?',
    'If courts are captured, the stewardship reading becomes a Snare (pure extraction with coordination cover) rather than a Tangled Rope—Indigenous nations have no actual recourse. If courts retain autonomy, the Tangled Rope classification holds; the asymmetry is enforced, not masked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_state_capture_of_interpretation, empirical, 'Whether settler institutional control over treaty interpretation forecloses genuine stewardship covenant enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__stewardship_reading, 1960, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1960, historical_treaty_substrate__stewardship_reading, theater_ratio, 1960, 0.35).
narrative_ontology:measurement_basis(hist_tr_t1960, observed).
narrative_ontology:measurement(hist_tr_t1980, historical_treaty_substrate__stewardship_reading, theater_ratio, 1980, 0.42).
narrative_ontology:measurement_basis(hist_tr_t1980, observed).
narrative_ontology:measurement(hist_tr_t2000, historical_treaty_substrate__stewardship_reading, theater_ratio, 2000, 0.52).
narrative_ontology:measurement_basis(hist_tr_t2000, observed).
narrative_ontology:measurement(hist_tr_t2015, historical_treaty_substrate__stewardship_reading, theater_ratio, 2015, 0.58).
narrative_ontology:measurement_basis(hist_tr_t2015, observed).
narrative_ontology:measurement(hist_tr_t2025, historical_treaty_substrate__stewardship_reading, theater_ratio, 2025, 0.62).
narrative_ontology:measurement_basis(hist_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(hist_be_t1960, historical_treaty_substrate__stewardship_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement_basis(hist_be_t1960, observed).
narrative_ontology:measurement(hist_be_t1980, historical_treaty_substrate__stewardship_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement_basis(hist_be_t1980, observed).
narrative_ontology:measurement(hist_be_t2000, historical_treaty_substrate__stewardship_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement_basis(hist_be_t2000, observed).
narrative_ontology:measurement(hist_be_t2015, historical_treaty_substrate__stewardship_reading, base_extractiveness, 2015, 0.3).
narrative_ontology:measurement_basis(hist_be_t2015, observed).
narrative_ontology:measurement(hist_be_t2025, historical_treaty_substrate__stewardship_reading, base_extractiveness, 2025, 0.31).
narrative_ontology:measurement_basis(hist_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1960, historical_treaty_substrate__stewardship_reading, suppression_requirement, 1960, 0.72).
narrative_ontology:measurement_basis(hist_su_t1960, observed).
narrative_ontology:measurement(hist_su_t1980, historical_treaty_substrate__stewardship_reading, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement_basis(hist_su_t1980, observed).
narrative_ontology:measurement(hist_su_t2000, historical_treaty_substrate__stewardship_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement_basis(hist_su_t2000, observed).
narrative_ontology:measurement(hist_su_t2015, historical_treaty_substrate__stewardship_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement_basis(hist_su_t2015, observed).
narrative_ontology:measurement(hist_su_t2025, historical_treaty_substrate__stewardship_reading, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement_basis(hist_su_t2025, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1960, tn=2025
narrative_ontology:measurement(hist_grid_01, historical_treaty_substrate__stewardship_reading, accessibility_collapse(class), 1960, 0.7).
narrative_ontology:measurement(hist_grid_02, historical_treaty_substrate__stewardship_reading, accessibility_collapse(class), 2025, 0.42).
narrative_ontology:measurement(hist_grid_03, historical_treaty_substrate__stewardship_reading, accessibility_collapse(individual), 1960, 0.62).
narrative_ontology:measurement(hist_grid_04, historical_treaty_substrate__stewardship_reading, accessibility_collapse(individual), 2025, 0.38).
narrative_ontology:measurement(hist_grid_05, historical_treaty_substrate__stewardship_reading, accessibility_collapse(organizational), 1960, 0.78).
narrative_ontology:measurement(hist_grid_06, historical_treaty_substrate__stewardship_reading, accessibility_collapse(organizational), 2025, 0.48).
narrative_ontology:measurement(hist_grid_07, historical_treaty_substrate__stewardship_reading, accessibility_collapse(structural), 1960, 0.85).
narrative_ontology:measurement(hist_grid_08, historical_treaty_substrate__stewardship_reading, accessibility_collapse(structural), 2025, 0.52).
narrative_ontology:measurement(hist_grid_09, historical_treaty_substrate__stewardship_reading, resistance(class), 1960, 0.4).
narrative_ontology:measurement(hist_grid_10, historical_treaty_substrate__stewardship_reading, resistance(class), 2025, 0.72).
narrative_ontology:measurement(hist_grid_11, historical_treaty_substrate__stewardship_reading, resistance(individual), 1960, 0.35).
narrative_ontology:measurement(hist_grid_12, historical_treaty_substrate__stewardship_reading, resistance(individual), 2025, 0.7).
narrative_ontology:measurement(hist_grid_13, historical_treaty_substrate__stewardship_reading, resistance(organizational), 1960, 0.32).
narrative_ontology:measurement(hist_grid_14, historical_treaty_substrate__stewardship_reading, resistance(organizational), 2025, 0.75).
narrative_ontology:measurement(hist_grid_15, historical_treaty_substrate__stewardship_reading, resistance(structural), 1960, 0.25).
narrative_ontology:measurement(hist_grid_16, historical_treaty_substrate__stewardship_reading, resistance(structural), 2025, 0.68).
narrative_ontology:measurement(hist_grid_17, historical_treaty_substrate__stewardship_reading, stakes_inflation(class), 1960, 0.32).
narrative_ontology:measurement(hist_grid_18, historical_treaty_substrate__stewardship_reading, stakes_inflation(class), 2025, 0.55).
narrative_ontology:measurement(hist_grid_19, historical_treaty_substrate__stewardship_reading, stakes_inflation(individual), 1960, 0.28).
narrative_ontology:measurement(hist_grid_20, historical_treaty_substrate__stewardship_reading, stakes_inflation(individual), 2025, 0.48).
narrative_ontology:measurement(hist_grid_21, historical_treaty_substrate__stewardship_reading, stakes_inflation(organizational), 1960, 0.38).
narrative_ontology:measurement(hist_grid_22, historical_treaty_substrate__stewardship_reading, stakes_inflation(organizational), 2025, 0.62).
narrative_ontology:measurement(hist_grid_23, historical_treaty_substrate__stewardship_reading, stakes_inflation(structural), 1960, 0.45).
narrative_ontology:measurement(hist_grid_24, historical_treaty_substrate__stewardship_reading, stakes_inflation(structural), 2025, 0.68).
narrative_ontology:measurement(hist_grid_25, historical_treaty_substrate__stewardship_reading, suppression(class), 1960, 0.68).
narrative_ontology:measurement(hist_grid_26, historical_treaty_substrate__stewardship_reading, suppression(class), 2025, 0.58).
narrative_ontology:measurement(hist_grid_27, historical_treaty_substrate__stewardship_reading, suppression(individual), 1960, 0.6).
narrative_ontology:measurement(hist_grid_28, historical_treaty_substrate__stewardship_reading, suppression(individual), 2025, 0.52).
narrative_ontology:measurement(hist_grid_29, historical_treaty_substrate__stewardship_reading, suppression(organizational), 1960, 0.75).
narrative_ontology:measurement(hist_grid_30, historical_treaty_substrate__stewardship_reading, suppression(organizational), 2025, 0.62).
narrative_ontology:measurement(hist_grid_31, historical_treaty_substrate__stewardship_reading, suppression(structural), 1960, 0.82).
narrative_ontology:measurement(hist_grid_32, historical_treaty_substrate__stewardship_reading, suppression(structural), 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__stewardship_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(historical_treaty_substrate__stewardship_reading, 0.18).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__nation_to_nation_reading).

% DUAL FORMULATION NOTE:
% The historical_treaty_substrate kernel produces three structurally distinct constraint readings. This file (stewardship_reading) interprets treaties as covenantal stewardship pacts with ongoing Indigenous consent authority. The sibling extinguishment_reading reads treaties as completed property transfers severing Indigenous territorial claims. The nation_to_nation_reading reads treaties as international agreements between sovereigns requiring renegotiation under modern treaty law. All three readings share the same textual kernel but instantiate different constraints because their ε values, beneficiary/victim structures, and typologies differ. Each reading's ε referent is the standing treaty arrangement under contest, assessed by that reading's own lights—not the alternative readings' endorsed arrangements. The three constraints form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
