% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__honorific_similarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__honorific_similarity_reading, []).

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
 *   constraint_id: homoousios_nicene__honorific_similarity_reading
 *   human_readable: Nicene Homoousios — Honorific Similarity Reading (Unity of Honor without Ontological Reduction)
 *   domain: historical theology / ecclesiastical history / philosophy of religion
 *
 * SUMMARY:
 *   The Nicene term homoousios ('of the same substance') is a contested
 *   kernel: one creedal commitment that the church's parties read
 *   differently. This story instantiates the honorific_similarity_reading —
 *   the term as a confession of unity of honor and worship, blur-adjacent to
 *   homoiousios, without commitment to strict metaphysical identity and
 *   without the hard derivation claims of subordinationism. The standing
 *   arrangement under contest (the ε referent) is the doctrinal boundary as
 *   this reading holds it across roughly 325–381: bishops confess the term,
 *   regional synods police its use, and the reading's flexibility lets a
 *   broad episcopal middle hold communion together while both wings — strict
 *   identity enforcers and hard subordinationists — pay in charges, sees, and
 *   exile. The interval maps T=0 to Nicaea 325 and T=30 to the
 *   Constantinopolitan settlement of 381 (grid points 0/6/12/18/24/30 ≈
 *   325/337/349/361/373/381), when the metaphysical reading's victory
 *   displaced this one. Claim and metrics are authored independently: the
 *   claim is tangled_rope (genuine coordination function, asymmetric
 *   extraction, active enforcement); the metrics describe the arrangement's
 *   full arc, including its decay into equivocal signature by interval end.
 *   The sibling readings are separate constraint files linked via
 *   network.affects_constraints; their ε values differ from this one's
 *   because they are different constraints, not different views of one.
 *
 * KEY AGENTS:
 *   - homoiousian_moderates: primary beneficiary (organized/constrained) — keeps sees and teaching office under the likeness gloss
 *   - apophatic_theologians: beneficiary (moderate/identity_locked) — spared metaphysical specification they regard as overreach
 *   - local_pastoral_bishops: beneficiary and co-administrator (organized/constrained) — collect interpretive discretion and the peace the formula buys
 *   - strict_nicene_enforcers: primary target (organized/identity_locked) — charged with rigidity, exiled, structurally unable to equivocate
 *   - hard_subordinationists: primary target (organized/constrained) — charged with heresy, squeezed by the unity formula
 *   - imperial_authority: agenda setter (powerful/arbitrage) — enforces whichever formula holds communion, shifts among readings at will
 *   - baptismal_laity: excluded voice (powerless/trapped) — inherits a renegotiated creed without a seat in the synods
 *   - ecclesiastical_historians: analytical observer — sees the whole structure and its outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__honorific_similarity_reading, 0.35).
domain_priors:suppression_score(homoousios_nicene__honorific_similarity_reading, 0.32).
domain_priors:theater_ratio(homoousios_nicene__honorific_similarity_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__honorific_similarity_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__honorific_similarity_reading, "Nicene Homoousios — Honorific Similarity Reading (Unity of Honor without Ontological Reduction)").
narrative_ontology:topic_domain(homoousios_nicene__honorific_similarity_reading, "historical theology / ecclesiastical history / philosophy of religion").

domain_priors:requires_active_enforcement(homoousios_nicene__honorific_similarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__honorific_similarity_reading, '04e28c92-69b1-40cc-be4a-2ea497ddb092').
narrative_ontology:cs_kernel_codification('04e28c92-69b1-40cc-be4a-2ea497ddb092', fixed_text).
narrative_ontology:cs_authority_grounding('04e28c92-69b1-40cc-be4a-2ea497ddb092', practice).
narrative_ontology:cs_interpretation_layer_present('04e28c92-69b1-40cc-be4a-2ea497ddb092').
narrative_ontology:cs_reading_relation('04e28c92-69b1-40cc-be4a-2ea497ddb092', homoousios_nicene__metaphysical_equality_reading, coexists_with).
narrative_ontology:cs_reading_relation('04e28c92-69b1-40cc-be4a-2ea497ddb092', homoousios_nicene__subordinationist_reading, influences).
narrative_ontology:cs_axiom('04e28c92-69b1-40cc-be4a-2ea497ddb092', foundational, homoousios_honorific_not_definitional).
narrative_ontology:cs_axiom_status(homoousios_honorific_not_definitional, holdable).
narrative_ontology:cs_axiom_grounding('04e28c92-69b1-40cc-be4a-2ea497ddb092', homoousios_honorific_not_definitional, conventional).
narrative_ontology:cs_axiom('04e28c92-69b1-40cc-be4a-2ea497ddb092', foundational, divine_essence_beyond_specification).
narrative_ontology:cs_axiom_status(divine_essence_beyond_specification, holdable).
narrative_ontology:cs_axiom_grounding('04e28c92-69b1-40cc-be4a-2ea497ddb092', divine_essence_beyond_specification, deontological).
narrative_ontology:cs_axiom('04e28c92-69b1-40cc-be4a-2ea497ddb092', secondary, term_unscriptural_must_be_rejected).
narrative_ontology:cs_axiom_status(term_unscriptural_must_be_rejected, overridden).
narrative_ontology:cs_axiom_grounding('04e28c92-69b1-40cc-be4a-2ea497ddb092', term_unscriptural_must_be_rejected, conventional).
narrative_ontology:cs_reference_frame('04e28c92-69b1-40cc-be4a-2ea497ddb092', honorific_unity_settlement).
narrative_ontology:cs_drift_state('04e28c92-69b1-40cc-be4a-2ea497ddb092', constantinopolitan_settlement, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('04e28c92-69b1-40cc-be4a-2ea497ddb092', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__honorific_similarity_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, homoiousian_moderates).
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, apophatic_theologians).
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, local_pastoral_bishops).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, strict_nicene_enforcers).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, hard_subordinationists).
narrative_ontology:constraint_vindicates(homoousios_nicene__honorific_similarity_reading, honorific_worship_parity).
narrative_ontology:constraint_vindicates(homoousios_nicene__honorific_similarity_reading, apophatic_reserve_on_divine_essence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The episcopal bloc around Basil of Ancyra and the Council of Ancyra (358) that reads the shared creedal term as 'of like substance.' Under the honorific gloss they keep their sees, their teaching office, and their scriptural method intact; a definition fixing strict identity would brand them heretics, and a hard derivation formula would absorb them into the wing they reject. Their ongoing cost is vigilance: the gloss must be defended at every synod against both wings.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, homoiousian_moderates, beneficiary,
    organized, generational, constrained, regional).

% Bishops and teachers who hold that the divine essence lies beyond specification. The honorific reading lets them confess the term alongside their congregations without asserting a metaphysics they regard as overreach. Exit is closed from both sides: signing a strict-identity definition would betray their epistemic commitments, and joining the derivation-teachers would betray their practice of giving the Son equal honor in worship.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, apophatic_theologians, beneficiary,
    moderate, generational, identity_locked, regional).

% The episcopal mainstream who administer the formula in their sees. They decide what unity of honor means for their congregations, sign the synodal letters that police the wings, and collect the resulting peace — secure sees, intact communion, and growing interpretive discretion as authority shifts from fixed definition to pastoral judgment. Their costs are enforcement effort and the risk of being caught between the wings at the next council.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, local_pastoral_bishops, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__honorific_similarity_reading, local_pastoral_bishops, agenda_setter).

% The Athanasian party and its allies, who hold that the creedal term means full identity of essence and that its meaning is not negotiable. Under the honorific settlement they are charged with rigidity and Sabellianism, lose sees to deposition and exile — Athanasius's repeated exiles are the emblem — and cannot sign the formulas in good conscience, because their entire case is that equivocation is betrayal. Leaving would require surrendering the claim they exist to defend.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, strict_nicene_enforcers, payer,
    organized, generational, identity_locked, continental).

% Teachers in the Arian and Anomoean lineage who hold that the Son derives being from the Father. The unity formula condemns them as heretics while occupying the scriptural ground of derivation they claim; they pay in convictions, exclusion from communion, and loss of imperial favor, and survive through retreat networks, sympathetic courts, and congregations that keep their teaching.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, hard_subordinationists, payer,
    organized, generational, constrained, regional).

% The emperor, who needs one church for one empire. He backs whichever formula holds communion together, enforces synodal decisions with exile and deposition, and can shift among readings at will — Constantius's sequence of settlements is the pattern. He collects domestic peace and pays enforcement costs; his commitment to any particular reading of the term is shallow.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, imperial_authority, agenda_setter,
    powerful, biographical, arbitrage, continental).

% Congregants baptized under creeds that predate the controversy. They would object that their inherited confession is being renegotiated over their heads, but they hold no seat in the synods; their voice reaches the record as acclamation, petition, and riot — the Constantinople 360 demonstrations — rather than as authored formula.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, baptismal_laity, excluded,
    powerless, biographical, trapped, regional).

% Later historians and creedal assessors with access to the whole record — acta, polemic, exile lists, and the eventual outcome. They can see which reading won, what the equivocation cost each wing, and how the term's meaning migrated. They collect nothing and pay nothing under the arrangement.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, ecclesiastical_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__honorific_similarity_reading, local_pastoral_bishops).
narrative_ontology:fixing_cost_class(homoousios_nicene__honorific_similarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds a single eucharistic communion across an unresolved metaphysical dispute: one confession term, read flexibly enough that a broad episcopal middle can sign it while meaning likeness, honor, or reserved judgment. It solves the collective-action problem of church unity at a moment when the essence question cannot be settled to anyone's satisfaction.
% TRANSFER_FUNCTION: Moves doctrinal compliance and discipline from both wings toward the episcopal center: strict identity enforcers pay in charges of rigidity, deposition, and exile; hard subordinationists pay in heresy convictions and exclusion; interpretive authority and secure sees flow to the moderate bishops who administer the formula, with the emperor collecting domestic peace.
% ABSENT_VOICES: The baptismal laity — congregants whose inherited confession predates the controversy — would object that their creed is being renegotiated over their heads; they appear in the record as acclamation and riot rather than as signatories. Exiled strict Nicenes and condemned subordinationists are likewise outside the synods that author the formulas; their objections survive in polemic, not in the acta.
% DISAPPEARANCE_RATIONALE: Without the honorific boundary, communion fractures along the very wings it holds together: strict-Nicene and subordinationist communions separate, sees reallocate by force, and the imperial peace the formula purchases disappears — approximately what happened once the reading lost the field after 381 and the homoian-style communions were suppressed.
% FOUNDING_PROBLEM: The term adopted at Nicaea in 325 to exclude Arius was itself suspect — condemned at Antioch in 268 as Sabellian-tending and absent from scripture — and the church needed to confess the Son's full honor in worship without either collapsing Father and Son into one person or dividing the divinity between them.
% FOUNDING_PROBLEM_CORROBORATION: Both victim wings attest the problem was real while disputing this reading's answer: Athanasian polemic (De Decretis) and subordinationist teaching (Aetius, Eunomius) both treat the term's ambiguity as the live crisis. Pre-controversy acta (the Antiochene condemnation of 268) and imperial correspondence (Constantine's and Constantius's directives on communion unity) corroborate the problem from outside the beneficiary set. No source outside the contesting parties attests that the honorific reading specifically solves it.
narrative_ontology:disappearance_verdict(homoousios_nicene__honorific_similarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__honorific_similarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__honorific_similarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_nicene__honorific_similarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__honorific_similarity_reading, 0.35, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__honorific_similarity_reading_tests).
:- end_tests(homoousios_nicene__honorific_similarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (ε = 0.35 at interval end, peaking 0.60 around T≈361) reflects an arrangement that taxes both doctrinal wings: the strict Nicene wing pays in rigidity charges, deposition, and exile; the subordinationist wing pays in heresy convictions and exclusion. The arc is a life cycle, not a ratchet: enforcement rose with imperial backing under Constantius (coerced signatures, exile of the intransigent), peaked near T=18, then decayed after 361 and collapsed after 381 as the metaphysical reading took the field. Theater rises monotonically (0.22 → 0.52): as the reading lost legitimacy, confession under the likeness gloss became increasingly equivocal performance — bishops signing formulas they read differently, the blur itself becoming the point. Suppression_requirement is authored because this story specifically tracks enforcement-capacity change: it builds (0.38 → 0.62) with the imperial-enforcement era and decays (→ 0.32) as the machinery loses its sponsor; suppression is authored as a raw structural property and is not scaled by power or scope. Accessibility_collapse is low (0.40) because the sibling readings stayed live throughout — this arrangement never closed the alternatives, it taxed them. Resistance (0.55) is real and two-sided. The primary coordination function is communion-boundary maintenance, hence boltzmann.coordination_type = identity_coordination with the type-default floor. Note the failed-coalition structure: the two victim wings could not combine against the arrangement because their premises are mutually opposed — the middle position's durability partly consists in that impossibility.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats compute a different arrangement than the beneficiary seats. From the strict Nicene seat, the honorific reading is an enforced equivocation that criminalizes precision and punishes fidelity to the term's plain force. From the subordinationist seat, it is a unity formula that occupies derivation's scriptural ground and brands its teachers heretics. From the moderate and apophatic seats, the same structure is the only workable peace: worship unity without speculative overreach, administered with pastoral judgment. The imperial seat experiences it as interchangeable settlement technology. The engine computes these per-seat classifications from the structural data (power, exit options, role); the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (homoiousian_moderates, apophatic_theologians, local_pastoral_bishops) drive d toward the beneficiary end for those seats; the local bishops' secondary administrator role does not reverse their net position — they collect discretion and peace and pay only enforcement effort. The victim declarations (strict_nicene_enforcers, hard_subordinationists) drive d toward the target end, amplified for the strict Nicenes by identity_locked exit: an agent who cannot sign in good conscience and cannot leave without betraying its core claim sits near the full-target end, so effective extraction concentrates there even at moderate base ε. The imperial authority's arbitrage exit places it near the beneficiary end despite its enforcement costs — it can re-opt among readings, which no episcopal seat can. Scope at the enforcement peak is continental, which the engine scales into effective extraction modestly; suppression remains unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Calling the honorific settlement pure coordination would erase what the coordination cost the wings — charges, exile, occupied scriptural ground — so the tangled_rope claim keeps the asymmetric payment visible inside the coordination. Calling it pure extraction would erase the real function: for five decades it held one eucharistic communion across an unresolvable metaphysical dispute, which no pure extraction device does. The founding problem is authored as contested rather than dead, so no capture/zombie mismatch fires; but the theater trajectory (0.22 → 0.52) marks the degradation risk: by interval end the arrangement persists increasingly as equivocal signature, and the omega honorific_blur_stability records whether that instability is intrinsic to the position or an artifact of its losing the field.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_delta_metaphysical_reading,
    'This story instantiates only the honorific_similarity_reading of the homoousios_nicene kernel; what structural delta would instantiating the metaphysical_equality_reading instead?',
    'Read against the sibling story homoousios_nicene__metaphysical_equality_reading: under the identity reading the victim set collapses to subordinationists alone, enforcement centralizes from local episcopal discretion into ecumenical conciliar definition, and base extractiveness rises — the post-381 settlement is the historical test case.',
    'If the sibling reading is instantiated, this reading''s beneficiaries (homoiousian moderates, apophatic traditions) move into the victim set, the boundary hardens from pastoral discretion to defined essence, and the coordination function narrows to enforcing a single metaphysics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_delta_metaphysical_reading, conceptual, 'Committer structure: structural delta of the metaphysical_equality sibling reading.').

omega_variable(
    sibling_delta_subordinationist_reading,
    'What structural delta would the subordinationist_reading instantiate for the same kernel?',
    'Read against the sibling story homoousios_nicene__subordinationist_reading: under the derivation reading the beneficiary/victim structure inverts — the moderate episcopal center and the strict Nicenes become the policed parties, subordinationist courts and teachers collect legitimacy, and the unity formula''s coordination function is replaced by hierarchical derivation ordering.',
    'Beneficiary and victim arrays invert; directionality for every named seat flips; the arrangement''s persistence would depend on hierarchical rather than conciliar enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_delta_subordinationist_reading, conceptual, 'Committer structure: structural delta of the subordinationist sibling reading.').

omega_variable(
    signature_equivocation_rate,
    'What share of bishops signing the honorific formulas privately held a strict-identity or subordinationist reading?',
    'Prosopographic tracking of signatories'' conduct after 361 and after 381: bishops who signed homoian-style formulas and promptly rejoined Nicene communion under Theodosius reveal equivocal signature; those who defended the gloss consistently reveal conviction.',
    'A high equivocation rate means theater_ratio is understated and the coordination function thinner than measured; a low rate confirms a genuine moderate constituency and supports the tangled coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signature_equivocation_rate, empirical, 'How much of the formula''s support was conviction versus equivocal signature.').

omega_variable(
    apophatic_benefit_timing,
    'Did apophatic theology benefit within this reading''s interval, or is its flourishing a product of the post-381 metaphysical settlement?',
    'Date the major apophatic production (the Cappadocian strain, Evagrius) against settlement dates; if it clusters after 381, the benefit is retrospective rather than contemporaneous.',
    'If retrospective, apophatic_theologians should be removed from the beneficiary set, the coordination function narrows to moderate-episcopal peace-keeping, and effective extraction on the remaining seats rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apophatic_benefit_timing, empirical, 'Whether the apophatic beneficiary seat is contemporaneous or anachronistic.').

omega_variable(
    enforcement_authorship_ambiguity,
    'How much of the measured suppressive force is imperial coercion rather than episcopal consensus?',
    'Compare suppression levels in reigns of doctrinally engaged emperors (Constantius) against disengaged or divided periods (post-361): if suppression tracks imperial attention, the arrangement''s own coercive core is smaller than the peak measurements suggest.',
    'If imperial-driven, the arrangement''s intrinsic suppression drops and its coordination character strengthens, shifting the reading toward the rope end; if episcopal, the enforcement is endogenous and the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_authorship_ambiguity, empirical, 'Attribution of suppressive force between imperial and episcopal sources.').

omega_variable(
    honorific_blur_stability,
    'Is ''likeness or honor without ontological reduction'' a stable third position, or an unstable equivocation that must eventually collapse into one of the sibling readings?',
    'Historical outcome plus conceptual analysis: the reading lost the field by 381 and its constituency was absorbed into the metaphysical settlement; conceptually, a term that affirms unity while refusing to specify its ground invites each party to read its own metaphysics into it.',
    'If intrinsically unstable, this reading is a transitional phase within the kernel''s life rather than a steady-state arrangement — its metrics are phase averages, its end-state theater reflects collapse rather than decay of a durable structure, and classification of any single moment understates the drift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(honorific_blur_stability, conceptual, 'Whether the honorific blur is a stable position or a transitional equivocation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__honorific_similarity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homoousios_honorific_tr_t0, homoousios_nicene__honorific_similarity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(homoousios_honorific_tr_t0, observed).
narrative_ontology:measurement(homoousios_honorific_tr_t6, homoousios_nicene__honorific_similarity_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement_basis(homoousios_honorific_tr_t6, observed).
narrative_ontology:measurement(homoousios_honorific_tr_t12, homoousios_nicene__honorific_similarity_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement_basis(homoousios_honorific_tr_t12, observed).
narrative_ontology:measurement(homoousios_honorific_tr_t18, homoousios_nicene__honorific_similarity_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement_basis(homoousios_honorific_tr_t18, observed).
narrative_ontology:measurement(homoousios_honorific_tr_t24, homoousios_nicene__honorific_similarity_reading, theater_ratio, 24, 0.46).
narrative_ontology:measurement_basis(homoousios_honorific_tr_t24, observed).
narrative_ontology:measurement(homoousios_honorific_tr_t30, homoousios_nicene__honorific_similarity_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement_basis(homoousios_honorific_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(homoousios_honorific_be_t0, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(homoousios_honorific_be_t0, observed).
narrative_ontology:measurement(homoousios_honorific_be_t6, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement_basis(homoousios_honorific_be_t6, observed).
narrative_ontology:measurement(homoousios_honorific_be_t12, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement_basis(homoousios_honorific_be_t12, observed).
narrative_ontology:measurement(homoousios_honorific_be_t18, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 18, 0.6).
narrative_ontology:measurement_basis(homoousios_honorific_be_t18, observed).
narrative_ontology:measurement(homoousios_honorific_be_t24, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 24, 0.46).
narrative_ontology:measurement_basis(homoousios_honorific_be_t24, observed).
narrative_ontology:measurement(homoousios_honorific_be_t30, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement_basis(homoousios_honorific_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(homoousios_honorific_su_t0, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(homoousios_honorific_su_t0, observed).
narrative_ontology:measurement(homoousios_honorific_su_t6, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement_basis(homoousios_honorific_su_t6, observed).
narrative_ontology:measurement(homoousios_honorific_su_t12, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(homoousios_honorific_su_t12, observed).
narrative_ontology:measurement(homoousios_honorific_su_t18, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement_basis(homoousios_honorific_su_t18, observed).
narrative_ontology:measurement(homoousios_honorific_su_t24, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 24, 0.46).
narrative_ontology:measurement_basis(homoousios_honorific_su_t24, observed).
narrative_ontology:measurement(homoousios_honorific_su_t30, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 30, 0.32).
narrative_ontology:measurement_basis(homoousios_honorific_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__honorific_similarity_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_nicene__honorific_similarity_reading, homoousios_nicene__metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__honorific_similarity_reading, homoousios_nicene__subordinationist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Nicene homoousios' covers three structurally distinct constraints — one per reading of the kernel. This story (honorific_similarity_reading) authors ε for the honorific boundary as its own reading holds it; the siblings author ε for the identity definition and the derivation reading respectively. Their ε values differ because they are different constraints, not different observables on one. The metaphysical_equality_reading is downstream in legitimacy: after 381 it absorbs this reading's constituency and inherits its enforcement machinery, so this story links to it as the reading that displaced it. The subordinationist_reading is linked as the wing this reading's boundary actively policed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
