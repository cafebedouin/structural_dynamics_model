% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__rangatiratanga_retention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__rangatiratanga_retention_reading, []).

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
 *   constraint_id: treaty_authority_cession__rangatiratanga_retention_reading
 *   human_readable: Treaty Partnership via Māori Text Control and Ongoing Consent
 *   domain: constitutional/indigenous_rights
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) is read through the lens of Māori text
 *   control via contra proferentem doctrine. Under this reading, the Māori
 *   text (which hapū chiefs understood and signed) controls interpretation;
 *   'kāwanatanga' is construed narrowly as governance authority only; 'tino
 *   rangatiratanga' (absolute chieftainship/sovereignty) is retained by hapū.
 *   The treaty establishes a partnership requiring Crown to seek ongoing hapū
 *   consent for laws and policies affecting hapū interests. This reading is
 *   structurally a Rope of genuine coordination—both parties exercise
 *   authority, neither can unilaterally override the other—but is shadowed by
 *   a retrospective Snare: the English text's mistranslation allowed
 *   systematic land alienation that hapū signatories never agreed to,
 *   revealing the constraint as operating under asymmetric information and
 *   coercive pressure once the translation fraud is visible.
 *
 * KEY AGENTS:
 *   - hapū collective authority: organizes tino rangatiratanga exercise; collective decision-making over lands and taonga; bound by identity_locked exit (hapū identity inseparable from land and authority)
 *   - the Crown: holds kāwanatanga (narrow governance); institutional power but constrained by partnership requirement and Māori text primacy
 *   - courts and legal interpreters: apply contra proferentem rule and Māori text control; gate the constraint's legitimacy
 *   - English text constituency (Crown officials, settlers): advocates for full sovereignty cession; excluded from this reading's frame
 *   - retrospective snare claimants (analysts, some hapū): argue the textual divergence itself is the extraction mechanism; centered in sibling constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.68).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.71).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Treaty Partnership via Māori Text Control and Ongoing Consent").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional/indigenous_rights").

domain_priors:requires_active_enforcement(treaty_authority_cession__rangatiratanga_retention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, 'b8e13a54-403d-41ce-8dfb-938a275c473d').
narrative_ontology:cs_kernel_codification('b8e13a54-403d-41ce-8dfb-938a275c473d', fixed_text).
narrative_ontology:cs_authority_grounding('b8e13a54-403d-41ce-8dfb-938a275c473d', lineage).
narrative_ontology:cs_interpretation_layer_present('b8e13a54-403d-41ce-8dfb-938a275c473d').
narrative_ontology:cs_reading_relation('b8e13a54-403d-41ce-8dfb-938a275c473d', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('b8e13a54-403d-41ce-8dfb-938a275c473d', treaty_authority_cession__biculturalism_reading, influences).
narrative_ontology:cs_reading_relation('b8e13a54-403d-41ce-8dfb-938a275c473d', treaty_authority_cession__retrospective_snare_exposure, coexists_with).
narrative_ontology:cs_axiom('b8e13a54-403d-41ce-8dfb-938a275c473d', foundational, maori_text_controls_interpretation).
narrative_ontology:cs_axiom_status(maori_text_controls_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('b8e13a54-403d-41ce-8dfb-938a275c473d', maori_text_controls_interpretation, conventional).
narrative_ontology:cs_axiom('b8e13a54-403d-41ce-8dfb-938a275c473d', foundational, tino_rangatiratanga_retained_inalienable).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_retained_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('b8e13a54-403d-41ce-8dfb-938a275c473d', tino_rangatiratanga_retained_inalienable, deontological).
narrative_ontology:cs_axiom('b8e13a54-403d-41ce-8dfb-938a275c473d', secondary, kawanatanga_limited_to_governance).
narrative_ontology:cs_axiom_status(kawanatanga_limited_to_governance, holdable).
narrative_ontology:cs_axiom_grounding('b8e13a54-403d-41ce-8dfb-938a275c473d', kawanatanga_limited_to_governance, conventional).
narrative_ontology:cs_axiom('b8e13a54-403d-41ce-8dfb-938a275c473d', foundational, partnership_requires_ongoing_hapu_consent).
narrative_ontology:cs_axiom_status(partnership_requires_ongoing_hapu_consent, holdable).
narrative_ontology:cs_axiom_grounding('b8e13a54-403d-41ce-8dfb-938a275c473d', partnership_requires_ongoing_hapu_consent, deontological).
narrative_ontology:cs_reference_frame('b8e13a54-403d-41ce-8dfb-938a275c473d', partnership_with_consent_requirement).
narrative_ontology:cs_drift_state('b8e13a54-403d-41ce-8dfb-938a275c473d', contemporary_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b8e13a54-403d-41ce-8dfb-938a275c473d', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, hapu_collective_authority).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, hapu_land_loss_under_mistranslation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, hapu_collective_authority).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, contra_proferentem_rule_applies_to_treaties).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, indigenous_text_controls_ambiguous_colonially_drafted_instruments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hapū exercise tino rangatiratanga (absolute authority) over lands, taonga, and cultural reproduction. They benefit from the constraint's partnership frame, which establishes Crown deference and requires consent for laws affecting hapū interests. They also pay the constraint: mobilizing collective decision-making, managing relationships with the Crown across multiple policy domains, and carrying the historical burden of land loss that occurred under mistranslation. Hapū are identity-locked to their lands and cannot exit without ceasing to exist as hapū.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, hapu_collective_authority, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, hapu_collective_authority, payer).

% The Crown holds kāwanatanga (governance authority) narrowly construed—administrative and legislative power exercisable only with hapū consent under partnership principles. The Crown can credibly commit to partnership (institutional legitimacy) or break it (prerogative power), making it the critical player in enforcement. The Crown is constrained by the partnership requirement and cannot unilaterally alienate hapū lands or override hapū interests in taonga. The Crown has mobile exit (could nominally withdraw from the treaty or reinterpret it) but faces Māori legal mobilization and international pressure if it attempts unilateral action.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, the_crown, agenda_setter,
    institutional, civilizational, mobile, national).

% Courts apply the contra proferentem rule (ambiguities resolve against the drafting party—the Crown) and privilege the Māori text when it diverges from the English text. Courts enforce partnership principles and constrain unilateral Crown action. Their interpretive authority is central to the constraint: they gate what counts as partnership, what constitutes adequate consent, and when the Crown breaches its obligations. Courts have analytical exit (can change jurisprudence) but face institutional pressure from government and from Māori legal mobilization.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, courts_and_legal_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% Crown officials, settler institutions, and political figures invested in the Crown cession reading are excluded from this constraint's legitimacy frame. They argue that the treaty was intended as complete cession of sovereignty, that 'kāwanatanga' means full Crown authority, and that the partnership reading is a judicial overreach constraining legitimate governance. They have not abandoned advocacy for their reading; they are excluded from this particular constraint narrative, not from power—they continue to pressure for Crown prerogative and against partnership requirements.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, english_text_constituency, excluded,
    powerful, biographical, constrained, national).

% Scholars and activists who argue that the textual divergence itself constitutes an extraction mechanism—that the signing of the Māori text by hapū chiefs could not constitute assent to the English text's sovereignty cession, and that land transfers and legislative override became visible as extraction operating under mistranslation once the fraud was exposed. They dispute the rope/partnership framing and see instead an ongoing Snare. They are excluded from this particular reading's frame (which takes the partnership reading as operative) but centered in the retrospective_snare_exposure constraint.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, retrospective_snare_analysts, excluded,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__rangatiratanga_retention_reading, diffuse).
narrative_ontology:fixing_cost_class(treaty_authority_cession__rangatiratanga_retention_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes durable, stable governance and land-management relationship between Crown and hapū: Crown provides administrative, legislative, defense, and trade infrastructure; hapū retain authority over cultural and land-management decisions, can enforce veto on laws affecting their interests, and participate in policy affecting taonga. Both parties coordinate rather than compete; Crown authority is legitimate only when exercised with hapū consent. The coordination solves the problem of how two sovereignties (Crown administrative authority, hapū traditional authority) can coexist and collaborate without one eliminating the other.
% TRANSFER_FUNCTION: The rope framing of this constraint does not describe a unidirectional transfer: it describes an authority allocation and a partnership mechanism. Under the partnership reading, the Crown transfers governance authority and Crown law-making power; hapū transfer recognition of Crown administrative role while retaining tino rangatiratanga. Both parties consent to the arrangement. Under the retrospective snare reading (the sibling constraint), the transfer was massive and one-directional: hapū lands moved to Crown control via English text enforcement while hapū signatories understood they were signing only governance delegation. In this rope story, no extractive transfer is the designed outcome—the constraint is supposed to prevent unilateral transfers.
% ABSENT_VOICES: Crown officials and settlers invested in the English text / full sovereignty cession reading are not parties to this reading's legitimacy frame; they would assert that the treaty was intended as complete cession and that the partnership reading is judicial invention. They are housed in the sibling crown_cession_reading constraint. Retrospective snare claimants (some hapū groups, some legal analysts) who argue that the textual divergence is itself the extraction mechanism are also largely absent from negotiating forums, though their analysis is present in academic and activist discourse. Individual hapū members (non-leadership) have historically had limited voice in collective decision-making; contemporary consultation processes try to broaden this but remain partial.
% DISAPPEARANCE_RATIONALE: If the partnership constraint vanished—if courts abandoned Māori text primacy and the partnership reading—the Crown would revert to unilateral authority over legislation, policy, and land management. Hapū would lose veto power and the institutional mechanism for enforcing consent. The entire landscape of indigenous rights, resource management, and constitutional law would reorganize around Crown prerogative rather than negotiated consent. Settlements would reverse, land claims would fail, and the 40+ year trajectory of Māori legal empowerment would collapse. This is not a marginal rearrangement; it is a foundational institutional reversal.
% FOUNDING_PROBLEM: The Treaty of Waitangi (1840) was drafted in English and Māori with textual divergences on core terms. The English version used 'cession of sovereignty'; the Māori version used 'kāwanatanga' (governance authority only), retaining 'tino rangatiratanga' (absolute chieftainship) for hapū. Hapū chiefs signed the Māori version and understood it to authorize Crown governance only, not land cession. The English version was later applied unilaterally to justify massive land alienation and Crown override of hapū interests. The founding problem is how to reconcile the two texts and recover the authority structure hapū actually consented to.
% FOUNDING_PROBLEM_CORROBORATION: Māori historians (Claudia Orange, Paul Moon, Paul Rutherford), legal scholars (James Belich, Hirini Moko Mead, Andrew Erueti), and the New Zealand Waitangi Tribunal have documented the textual divergence and the mistranslation of 'kāwanatanga' from the outset. The Tribunal's foundational findings (1989 onwards) established that the Māori text controls and that Crown interpretations of 'cession' violated the treaty as understood by Māori signatories. This corroboration comes from outside the Crown (the benefiting party under cession reading) and is grounded in archival evidence, Māori testimony, and comparative treaty jurisprudence. No Crown official at the time of drafting has been recorded as explicitly acknowledging the intentional divergence, though archival records suggest Crown awareness of the translation issue.
narrative_ontology:disappearance_verdict(treaty_authority_cession__rangatiratanga_retention_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__rangatiratanga_retention_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(treaty_authority_cession__rangatiratanga_retention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   At t0 (1840), extractiveness is near zero: the constraint is just authored, the Māori signatories understand it as partnership and sovereignty retention. By t=1880, extractiveness spikes to 0.72 as the Crown applies the English text unilaterally to alienate massive land holdings that hapū had never authorized under the Māori text reading—the snare is now operant, running on mistranslation and suppression (0.88). Extractiveness peaks at t=1950 (0.81) at the height of land dispossession. Theater ratio rises from near-zero to 0.35 by 1950: courts and Crown officials perform 'consulting' hapū while implementing predetermined outcomes; legal doctrine talks partnership while law operates cession. By t=2000, extractiveness drops to 0.52 as courts begin enforcing Māori text primacy and the Waitangi Tribunal constrains unilateral Crown action—the Rope reading starts to bite. At t=2026 extractiveness rises to 0.68: ongoing disputes over consent authority, resource management, and historical land claims show the constraint is still contested and still requires active enforcement to remain a partnership rather than reverting to Crown prerogative. Suppression requirement tracks extraction closely: it peaks when land alienation is highest (1950, 0.85) and declines as legal constraints tighten (2000, 0.61), but rises again at 2026 (0.71) as pressure to overturn partnership constraints intensifies. Resistance declines from 0.85 (1840, powerful initial hapū mobilization) to 0.52 (2026, routinized institutional engagement replaces grassroots resistance), indicating the constraint has hardened into institutional form but lost some grassroots force.
 *
 * PERSPECTIVAL GAP:
 *   From the hapū seat, this is a partnership in principle but asymmetrically enforced: the Crown regularly pushes consent requirements and reinterprets scope (mobile exit for the Crown, identity_locked for hapū). From the Crown seat, partnership is costly and constrains necessary policy; the institutional power atom makes the Crown believe it could operate unilaterally (mobile exit is a delusion when Māori legal mobilization is factored in). Courts sit between, enforcing the Māori text reading but working within a Crown-created institutional frame that still privileges Crown prerogative in ambiguous cases. The engine should compute different types across seats: hapū see Rope (genuine coordination with asymmetric enforcement); Crown sees constrained prerogative (a Rope it wishes were Mountains); courts see the machinery (a Rope requiring active interpretation to hold). The measurement series captures this as theater_ratio rising (courts performing partnership while Crown centralizes implementation control) and suppression_requirement staying high (the constraint requires ongoing coercive backing to prevent Crown reversion).
 *
 * DIRECTIONALITY LOGIC:
 *   Hapū are structural beneficiaries (retain tino rangatiratanga, can veto unilateral Crown action) and structural payers (carry administrative burden of collective decision-making, carry historical land loss, carry identity-lock that prevents exit). The Crown is the agenda-setter (enforces the Māori text reading, interprets consent requirements, can credibly commit to partnership or break it) but is constrained (cannot alienate hapū lands unilaterally, cannot legislate without consent on reserved matters). Courts are the enforcement machinery (apply contra proferentem, Māori text primacy, partnership principles). English text constituency is excluded: they would assert full sovereignty cession but that reading is authored as the sibling crown_cession_reading, a different constraint with different ε, different beneficiaries, different type. Directionality for hapū is near symmetric (0.45–0.55): genuine benefit from authority retention and veto power, genuine cost from enforcement burden and historical loss. Directionality for the Crown approaches the target end (0.6–0.7): constrained by partnership requirement, benefits from legitimacy (partnership is more durable than naked prerogative) but pays enforcement costs. No override needed: the structural derivation from beneficiary/victim declarations plus identity_locked exit for hapū maps correctly to directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (textual divergence allowing mistranslation of sovereignty cession) is LIVE and CONTESTED: hapū and courts assert it is unresolved, Crown often frames it as settled by 150+ years of practice (the snare logic: the crime became normal). The disappearance verdict is WORLD_REARRANGES: if the partnership reading vanished, the Crown would revert to unilateral prerogative and land alienation would resume. This creates the mandatrophy condition: the founding problem persists, the world would rearrange if the constraint were removed, yet the constraint requires constant reaffirmation through legal doctrine and institutional struggle—it is not a self-executing partnership but a hard-won legal fiction maintained against Crown drift. The theater_ratio rising to 0.48 indicates the Crown increasingly performs partnership while seeking to narrow its scope: consultation processes become theatrical cover for centralized decisions. This is the mandatrophy signature: the founding problem is unresolved (mistranslation never remedied, land never returned), the constraint persists by legal doctrine and Māori mobilization, not by natural evolution toward partnership, and performance is substituting for function as the Crown seeks to evade substantive consent requirements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contra_proferentem_empirical_scope,
    'Does contra proferentem (ambiguities resolve against the drafting party) apply to the Crown as author of the English text, or does it apply symmetrically to both versions because both were proposed for signature?',
    'Comparative treaty jurisprudence from other colonial contexts (Canada, Australia, US); courts'' own evolving doctrine on treaty interpretation authority.',
    'If contra proferentem applies asymmetrically (against Crown), the Māori text controls strongly and hapū retain unambiguous tino rangatiratanga. If applied symmetrically, ambiguities require balancing both texts, which softens the partnership constraint and allows Crown equivocation. This is the pivot for whether the reading is a Rope or a softer Rope-bordering-Tangled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contra_proferentem_empirical_scope, empirical, 'Scope and symmetry of the contra proferentem doctrine applied to treaty texts.').

omega_variable(
    collective_consent_operationalization,
    'What constitutes ''hapū consent'' for Crown policy? Individual hapū veto, majority of iwi, formal delegation to representatives, or something else?',
    'Historical practice of consent-seeking; court rulings on consultation adequacy; hapū and Crown negotiations over consent mechanisms.',
    'Narrow operationalization (individual veto) makes the constraint more extractive for the Crown (high suppression to prevent veto abuse) but more empowering for hapū. Broad operationalization (delegation to representatives) makes it less extractive but creates agency loss for individual hapū members. This drives directionality and power distribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_consent_operationalization, conceptual, 'How collective consent is operationalized in practice—a site of ongoing contestation.').

omega_variable(
    identity_lock_vs_mobility,
    'Can hapū exit the partnership constraint by ceding tino rangatiratanga to the Crown, or is tino rangatiratanga inalienable (identity-locked)?',
    'Hapū choices in settlement negotiations and land claims; constitutional doctrine on alienability of indigenous rights; international law on indigenous self-determination.',
    'If inalienable (identity-locked), hapū exit_options is trapped/identity_locked and directionality is pushed toward target end (higher extraction). If alienable, hapū could exit by choosing Crown sovereignty, which would shift the reading entirely. This tests whether the constraint is a Rope or a Snare by hidden identity-lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_mobility, preference, 'Whether tino rangatiratanga can be alienated or is constitutionally inalienable.').

omega_variable(
    textual_divergence_as_fraud_or_accident,
    'Was the textual divergence deliberate Crown deception (designed to allow later English text enforcement) or a translation accident that Crown officials later exploited?',
    'Archival evidence from treaty negotiation records; Crown memoranda on strategy; testimony from Māori signatories and translators; comparative analysis of similar colonial treaties.',
    'If deliberate, the constraint has always been a Snare (retrospective_snare_exposure reading applies from t0). If accident, the early period (t0–t1880) shows good-faith Rope, which devolves into Snare as Crown discovers the loophole and exploits it. This affects the historical trajectory interpretation and the legitimacy of the partnership frame.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_divergence_as_fraud_or_accident, empirical, 'Whether the textual divergence was intentional Crown strategy or historical accident later exploited.').

omega_variable(
    sibling_reading_foreclosure_vs_coexistence,
    'Do the rangatiratanga retention reading and the crown cession reading logically foreclose each other (one must be false if the other is true), or can they coexist as competing institutional and legal commitments held by different parties?',
    'Jurisprudential analysis of whether both readings can be held simultaneously within a single legal framework; court rulings on treaty supremacy and constitutional hierarchy; political settlements that embed both readings in institutional practice.',
    'If they foreclose each other, the constraint is a site of zero-sum competition and classification should emphasize the snare-exposure reading. If they coexist, the current state is genuinely a Rope of negotiated partnership operating despite underlying disagreement. This test determines whether the kernel contains a fundamental logical contradiction or a manageable pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_vs_coexistence, conceptual, 'Whether this reading''s core axioms logically foreclose the sibling crown cession reading or allow coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 1840, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1840, 0.0).
narrative_ontology:measurement_basis(trea_tr_t1840, projected).
narrative_ontology:measurement(trea_tr_t1880, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1880, 0.15).
narrative_ontology:measurement_basis(trea_tr_t1880, observed).
narrative_ontology:measurement(trea_tr_t1950, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1950, 0.35).
narrative_ontology:measurement_basis(trea_tr_t1950, observed).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1975, 0.42).
narrative_ontology:measurement_basis(trea_tr_t1975, observed).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2000, 0.51).
narrative_ontology:measurement_basis(trea_tr_t2000, observed).
narrative_ontology:measurement(trea_tr_t2026, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2026, 0.48).
narrative_ontology:measurement_basis(trea_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1840, 0.05).
narrative_ontology:measurement_basis(trea_be_t1840, projected).
narrative_ontology:measurement(trea_be_t1880, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1880, 0.72).
narrative_ontology:measurement_basis(trea_be_t1880, observed).
narrative_ontology:measurement(trea_be_t1950, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1950, 0.81).
narrative_ontology:measurement_basis(trea_be_t1950, observed).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1975, 0.68).
narrative_ontology:measurement_basis(trea_be_t1975, observed).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement_basis(trea_be_t2000, observed).
narrative_ontology:measurement(trea_be_t2026, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(trea_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1840, 0.0).
narrative_ontology:measurement_basis(trea_su_t1840, projected).
narrative_ontology:measurement(trea_su_t1880, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1880, 0.88).
narrative_ontology:measurement_basis(trea_su_t1880, observed).
narrative_ontology:measurement(trea_su_t1950, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1950, 0.85).
narrative_ontology:measurement_basis(trea_su_t1950, observed).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1975, 0.72).
narrative_ontology:measurement_basis(trea_su_t1975, observed).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2000, 0.61).
narrative_ontology:measurement_basis(trea_su_t2000, observed).
narrative_ontology:measurement(trea_su_t2026, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(trea_su_t2026, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1840, tn=2026
narrative_ontology:measurement(trea_grid_01, treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse(class), 1840, 0.15).
narrative_ontology:measurement(trea_grid_02, treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse(class), 2026, 0.65).
narrative_ontology:measurement(trea_grid_03, treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse(individual), 1840, 0.08).
narrative_ontology:measurement(trea_grid_04, treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse(individual), 2026, 0.6).
narrative_ontology:measurement(trea_grid_05, treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse(organizational), 1840, 0.05).
narrative_ontology:measurement(trea_grid_06, treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse(organizational), 2026, 0.58).
narrative_ontology:measurement(trea_grid_07, treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse(structural), 1840, 0.1).
narrative_ontology:measurement(trea_grid_08, treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse(structural), 2026, 0.62).
narrative_ontology:measurement(trea_grid_09, treaty_authority_cession__rangatiratanga_retention_reading, resistance(class), 1840, 0.75).
narrative_ontology:measurement(trea_grid_10, treaty_authority_cession__rangatiratanga_retention_reading, resistance(class), 2026, 0.55).
narrative_ontology:measurement(trea_grid_11, treaty_authority_cession__rangatiratanga_retention_reading, resistance(individual), 1840, 0.7).
narrative_ontology:measurement(trea_grid_12, treaty_authority_cession__rangatiratanga_retention_reading, resistance(individual), 2026, 0.52).
narrative_ontology:measurement(trea_grid_13, treaty_authority_cession__rangatiratanga_retention_reading, resistance(organizational), 1840, 0.8).
narrative_ontology:measurement(trea_grid_14, treaty_authority_cession__rangatiratanga_retention_reading, resistance(organizational), 2026, 0.62).
narrative_ontology:measurement(trea_grid_15, treaty_authority_cession__rangatiratanga_retention_reading, resistance(structural), 1840, 0.85).
narrative_ontology:measurement(trea_grid_16, treaty_authority_cession__rangatiratanga_retention_reading, resistance(structural), 2026, 0.58).
narrative_ontology:measurement(trea_grid_17, treaty_authority_cession__rangatiratanga_retention_reading, stakes_inflation(class), 1840, 0.25).
narrative_ontology:measurement(trea_grid_18, treaty_authority_cession__rangatiratanga_retention_reading, stakes_inflation(class), 2026, 0.78).
narrative_ontology:measurement(trea_grid_19, treaty_authority_cession__rangatiratanga_retention_reading, stakes_inflation(individual), 1840, 0.18).
narrative_ontology:measurement(trea_grid_20, treaty_authority_cession__rangatiratanga_retention_reading, stakes_inflation(individual), 2026, 0.72).
narrative_ontology:measurement(trea_grid_21, treaty_authority_cession__rangatiratanga_retention_reading, stakes_inflation(organizational), 1840, 0.15).
narrative_ontology:measurement(trea_grid_22, treaty_authority_cession__rangatiratanga_retention_reading, stakes_inflation(organizational), 2026, 0.7).
narrative_ontology:measurement(trea_grid_23, treaty_authority_cession__rangatiratanga_retention_reading, stakes_inflation(structural), 1840, 0.2).
narrative_ontology:measurement(trea_grid_24, treaty_authority_cession__rangatiratanga_retention_reading, stakes_inflation(structural), 2026, 0.75).
narrative_ontology:measurement(trea_grid_25, treaty_authority_cession__rangatiratanga_retention_reading, suppression(class), 1840, 0.05).
narrative_ontology:measurement(trea_grid_26, treaty_authority_cession__rangatiratanga_retention_reading, suppression(class), 2026, 0.74).
narrative_ontology:measurement(trea_grid_27, treaty_authority_cession__rangatiratanga_retention_reading, suppression(individual), 1840, 0.02).
narrative_ontology:measurement(trea_grid_28, treaty_authority_cession__rangatiratanga_retention_reading, suppression(individual), 2026, 0.7).
narrative_ontology:measurement(trea_grid_29, treaty_authority_cession__rangatiratanga_retention_reading, suppression(organizational), 1840, 0.0).
narrative_ontology:measurement(trea_grid_30, treaty_authority_cession__rangatiratanga_retention_reading, suppression(organizational), 2026, 0.68).
narrative_ontology:measurement(trea_grid_31, treaty_authority_cession__rangatiratanga_retention_reading, suppression(structural), 1840, 0.0).
narrative_ontology:measurement(trea_grid_32, treaty_authority_cession__rangatiratanga_retention_reading, suppression(structural), 2026, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__rangatiratanga_retention_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(treaty_authority_cession__rangatiratanga_retention_reading, 0.12).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__retrospective_snare_exposure).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__biculturalism_reading).

% DUAL FORMULATION NOTE:
% The treaty_authority_cession kernel is decomposed into multiple constraint stories, each instantiating a different reading of the Māori text / English text divergence. This story (rangatiratanga_retention_reading) privileges Māori text interpretation and establishes partnership as a binding structure. The crown_cession_reading privileges English text and frames the treaty as completing legal cession. The retrospective_snare_exposure reading exposes the textual divergence as an extraction mechanism operating through mistranslation. Each reading has a different ε (extraction from different seats), different beneficiary/victim structure, different type. They are linked via network.affects_constraints because the legal interpretation of one reading constrains the others: courts' adoption of Māori text primacy (this reading) forecloses or constrains the crown_cession reading's jurisprudential pathway.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__rangatiratanga_retention_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
