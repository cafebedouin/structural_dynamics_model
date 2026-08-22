% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__rangatiratanga_retention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Te Tiriti o Waitangi — Rangatiratanga Retention Reading
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This story instantiates the rangatiratanga-retention reading of the
 *   founding kernel of New Zealand's constitutional order: that the Māori
 *   text of Te Tiriti o Waitangi, which the overwhelming majority of
 *   rangatira signed, controls interpretation under contra proferentem
 *   (ambiguity resolved against the drafter, the Crown), that 'kāwanatanga'
 *   names a bounded governance function rather than sovereignty, and that
 *   'tino rangatiratanga' — full chiefly authority — was retained by hapū and
 *   iwi. Under this reading, the treaty is a partnership instrument: Crown
 *   action touching Māori interests is legitimate only insofar as it proceeds
 *   with hapū/iwi consent, not merely Crown say-so. This is the reading
 *   substantially adopted by the Waitangi Tribunal's principles jurisprudence
 *   and by much of the modern treaty settlement process. It is ONE of three
 *   declared readings of the kernel treaty_authority_cession — the
 *   crown_cession_reading (English text controls, full sovereignty ceded) and
 *   the retrospective_snare_exposure reading (textual divergence itself is
 *   the extraction mechanism) are separate constraints with their own ε
 *   values, not alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - hapu_and_iwi: primary rights-holder and, when the partnership functions, beneficiary; when breached, payer (organized/identity_locked)
 *   - crown_when_acting_with_consent: administers governance function, agenda-setter, benefits from legitimate stable partnership (institutional/constrained)
 *   - land_alienated_communities: bears the concrete historical cost of breach — dispossession without consent (powerless/trapped)
 *   - settler_descended_landholders: secondary beneficiary of historical breach, holds resulting property (moderate/mobile)
 *   - waitangi_tribunal: analytical/institutional observer whose jurisdiction is built substantially on this reading
 *   - crown_cession_reading_advocates: excluded from this reading's account, hold the sibling reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.58).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.62).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, tangled_rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Te Tiriti o Waitangi — Rangatiratanga Retention Reading").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__rangatiratanga_retention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, '33e15abd-50b7-49e0-b5b0-11643d1a80c1').
narrative_ontology:cs_kernel_codification('33e15abd-50b7-49e0-b5b0-11643d1a80c1', fixed_text).
narrative_ontology:cs_authority_grounding('33e15abd-50b7-49e0-b5b0-11643d1a80c1', lineage).
narrative_ontology:cs_interpretation_layer_present('33e15abd-50b7-49e0-b5b0-11643d1a80c1').
narrative_ontology:cs_reading_relation('33e15abd-50b7-49e0-b5b0-11643d1a80c1', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('33e15abd-50b7-49e0-b5b0-11643d1a80c1', treaty_authority_cession__retrospective_snare_exposure, influences).
narrative_ontology:cs_axiom('33e15abd-50b7-49e0-b5b0-11643d1a80c1', foundational, maori_text_controls_under_contra_proferentem).
narrative_ontology:cs_axiom_status(maori_text_controls_under_contra_proferentem, holdable).
narrative_ontology:cs_axiom_grounding('33e15abd-50b7-49e0-b5b0-11643d1a80c1', maori_text_controls_under_contra_proferentem, conventional).
narrative_ontology:cs_axiom('33e15abd-50b7-49e0-b5b0-11643d1a80c1', foundational, kawanatanga_names_bounded_governance_not_sovereignty).
narrative_ontology:cs_axiom_status(kawanatanga_names_bounded_governance_not_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('33e15abd-50b7-49e0-b5b0-11643d1a80c1', kawanatanga_names_bounded_governance_not_sovereignty, empirically_contingent).
narrative_ontology:cs_axiom('33e15abd-50b7-49e0-b5b0-11643d1a80c1', secondary, ongoing_consent_required_for_legitimate_crown_action).
narrative_ontology:cs_axiom_status(ongoing_consent_required_for_legitimate_crown_action, holdable).
narrative_ontology:cs_axiom_grounding('33e15abd-50b7-49e0-b5b0-11643d1a80c1', ongoing_consent_required_for_legitimate_crown_action, conventional).
narrative_ontology:cs_reference_frame('33e15abd-50b7-49e0-b5b0-11643d1a80c1', partnership_founding_bargain).
narrative_ontology:cs_drift_state('33e15abd-50b7-49e0-b5b0-11643d1a80c1', post_waitangi_tribunal_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('33e15abd-50b7-49e0-b5b0-11643d1a80c1', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, hapu_and_iwi).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, crown_when_acting_with_consent).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, hapu_and_iwi).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, land_alienated_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, settler_descended_landholders).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, contra_proferentem_doctrine).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, partnership_principle_of_treaty_relations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Signed Te Tiriti in Māori text, ceding kāwanatanga (governorship, a bounded administrative function) while retaining tino rangatiratanga (full chiefly authority over lands, villages, and treasured things). Under this reading they remain the rights-holding partner whose ongoing consent is required for legitimate Crown action. In practice they have repeatedly found Crown administration, legislation, and land transfer proceeding without that consent, and have carried the costs of enforcement action (petitions, litigation, protest) to make the retained authority operative rather than nominal. Exit from the relationship is not available — the partnership is constitutive of their political identity — so the only path is contesting how the kernel is read from within it.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, hapu_and_iwi, beneficiary,
    organized, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, hapu_and_iwi, payer).

% Holds the governance function (kāwanatanga) ceded under the Māori text: the capacity to make law and administer common affairs, but bounded by the requirement to secure hapū/iwi consent for actions touching rangatiratanga. When the Crown operates through negotiated settlement, co-governance arrangements, and Waitangi Tribunal processes, it exercises legitimate authority under this reading and benefits from a stable, workable partnership rather than a contested one. Its exit option is constrained, not free: it cannot unilaterally exit the partnership without forfeiting the legitimacy the treaty itself supplies as the foundational instrument of Crown authority in Aotearoa.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, crown_when_acting_with_consent, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, crown_when_acting_with_consent, beneficiary).

% Specific hapū whose lands were confiscated, sold under duress, or legislated away in the nineteenth and twentieth centuries without the consent this reading holds was constitutionally required. They bear the concrete, often irreversible costs of the gap between the retained-authority reading and what the Crown actually did — dispossession, loss of self-governing capacity, intergenerational economic harm. Their situation is the clearest evidence, under this reading, that partnership was breached rather than honoured; the Waitangi Tribunal exists substantially because of their claims.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, land_alienated_communities, payer,
    powerless, generational, trapped, regional).

% Hold title to land alienated from hapū under the historical breaches this reading identifies. They did not administer the treaty and mostly did not participate in the original bad-faith transactions, but they hold the resulting property benefit and have organized, mobile options (political voice, market alienability of land) that the dispossessed communities did not have at the time and often still lack.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, settler_descended_landholders, beneficiary,
    moderate, generational, mobile, national).

% A statutory body tasked with hearing claims of Crown breach of treaty principles, including principles derived substantially from this retention reading (partnership, active protection, redress). It takes evidence from hapū, historians, and Crown agencies, and issues findings that inform (but do not bind) settlement negotiations. Its existence is itself downstream of this reading gaining institutional traction from the 1970s onward.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Historical and some contemporary legal voices who hold that the English text, asserting cession of full sovereignty, controls, and that kāwanatanga in the Māori text was intended and understood to mean sovereignty. This reading is a sibling constraint, not part of this story's content, but its advocates are structurally excluded from THIS reading's account of what was ceded — they would object that treating the Māori text as controlling erases the Crown's own founding legal instrument.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, crown_cession_reading_advocates, excluded,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__rangatiratanga_retention_reading, diffuse).
narrative_ontology:fixing_cost_class(treaty_authority_cession__rangatiratanga_retention_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a bicultural constitutional partnership: the Crown gains a governance function to administer common affairs (kāwanatanga) among a growing settler population, while hapū and iwi retain authority over their own lands, resources, and internal affairs (tino rangatiratanga), with Crown action affecting Māori interests requiring ongoing consent rather than a one-time cession.
% TRANSFER_FUNCTION: Under a functioning reading, what moves is a bounded administrative competence from hapū/iwi to the Crown — not sovereignty, not land, not self-determination. Where the Crown has acted beyond that bounded competence (land confiscation, legislative override without consent), the actual historical transfer has been land, resource access, and governing capacity moving from hapū/iwi to the Crown and settler interests, which this reading identifies as breach rather than legitimate operation.
% ABSENT_VOICES: Advocates of the English-text cession reading are excluded from this reading's own account of what was agreed; they would argue this reading retrofits a partnership that the Crown never intended to honour as anything more than a formality. Hapū who experienced dispossession are not absent from this reading (they are central to it), but their voices were historically absent from the courts and legislature that adjudicated the treaty's meaning for over a century.
% DISAPPEARANCE_RATIONALE: If this reading of the treaty vanished from constitutional and legal discourse, the Waitangi Tribunal's principle-based jurisdiction would lose its doctrinal foundation, treaty settlements would lose their normative grounding, and co-governance arrangements would become politically unmoored — Māori claimants say the world rearranges toward unconstrained Crown authority; Crown-cession advocates say the underlying legal position (sovereignty already ceded) would simply be restated without this reading's overlay, so the world is largely unchanged from their standpoint. The parties dispute which is true, which is itself part of what the kernel contest is about.
% FOUNDING_PROBLEM: In 1840, competing British and French colonial interest, unregulated settler land purchasing, and lawlessness among arriving settlers created pressure for some form of British administrative presence; hapū and iwi rangatira sought protection of their authority and lands against these pressures while permitting a governance mechanism over settlers.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the Crown and outside Māori claimant interests by independent legal historians (e.g. Claudia Orange's documentary history) and by the Waitangi Tribunal's own historical findings, which draw on missionary records, contemporaneous Colonial Office correspondence, and oral history — sources that predate and sit outside both the modern Crown legal position and modern Māori claimant advocacy, and which broadly corroborate that the Māori-text signatories understood themselves to be retaining rangatiratanga rather than ceding it.
narrative_ontology:disappearance_verdict(treaty_authority_cession__rangatiratanga_retention_reading, contested).
narrative_ontology:founding_problem_status(treaty_authority_cession__rangatiratanga_retention_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(treaty_authority_cession__rangatiratanga_retention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.58 for the standing arrangement — under this reading, the treaty as a partnership instrument is not itself extractive (a genuine coordination structure with mutual obligation), but its historical operation has been substantially extractive where the Crown acted as though it held sovereignty rather than a bounded governance grant, particularly through nineteenth-century land confiscation and legislative override. Suppression at 0.62 reflects both the historical coercive machinery (native land courts, confiscation legislation, military action) used to override the consent requirement, and the ongoing structural difficulty hapū face in compelling consent-based process even now. Theater ratio (0.28) is moderate: contemporary settlement processes and co-governance arrangements have real substantive content, but a portion of Crown engagement functions as legitimacy performance without altering underlying land and resource control. Resistance is high (0.75) because hapū/iwi have never accepted breach as settled — petitioning, litigating, and organizing continuously since 1840 to make the retained-authority reading operative.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's agenda-setting seat (when acting within this reading's bounds), the arrangement looks like functioning coordination: a governance mandate exercised with appropriate deference to Māori authority. From the payer seats — hapū whose lands were alienated — the same historical structure looks like extraction wearing partnership's clothing: consent was sought only when convenient and overridden by legislation and force when it was not. The engine should compute these as structurally different seat experiences of one story, because they are: the same treaty relationship produces coordination where consent was honoured and produces extraction where it was not, and both have occurred under the same nominal instrument.
 *
 * DIRECTIONALITY LOGIC:
 *   Hapū and iwi carry a genuinely mixed directionality: as the retained rights-holder in a functioning partnership they sit toward the beneficiary end, but as the historical target of confiscation and override they sit toward the extraction-target end — hence dual roles (beneficiary + payer). The Crown, when honouring the consent requirement, sits toward mutual benefit; the historical record complicates this, which is why enforcement (suppression) is authored high even though the reading's coordination function is genuine. Land-alienated communities are the clearest full-target case: trapped exit, powerless at the time, bearing losses that in most cases have never been fully reversed. Settler-descended landholders are beneficiaries of the breach without having authored it, which is why their directionality does not equal the Crown's — they hold the asset, not the authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regulating settler conduct while protecting hapū authority) is authored as still live: co-governance disputes, resource management conflicts, and ongoing settlement negotiations show the underlying coordination need has not disappeared, even though its administrative form has changed radically since 1840. This blocks a mandatrophy reading that would treat the treaty relationship as an obsolete formality persisting only by inertia — the Waitangi Tribunal's continuing caseload and contemporary co-governance litigation are evidence the coordination function remains active, not merely ceremonial, even as the theater_ratio shows meaningful performative content coexisting with substance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    which_text_controls,
    'Does the Māori text or the English text control the treaty''s legal meaning, and does contra proferentem properly apply given the Crown drafted both versions?',
    'This is not empirically resolvable in the ordinary sense — it is a question of legal-interpretive doctrine and historical linguistic reconstruction (what rangatira signing the Māori text understood ''kāwanatanga'' and ''tino rangatiratanga'' to mean at the point of signing, informed by missionary translation practice, prior Māori political concepts, and comparative treaty language from the period).',
    'If the English-text/full-sovereignty reading is adopted instead, this constraint''s coordination framing collapses and the arrangement is better modeled as the sibling crown_cession_reading constraint — a different ε, different beneficiary/victim structure, and likely a harder extraction classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_text_controls, conceptual, 'The kernel''s central textual/interpretive ambiguity: which treaty text is authoritative.').

omega_variable(
    reading_as_retrofit_or_recovery,
    'Is the rangatiratanga-retention reading a recovery of what was actually agreed in 1840, or a modern doctrinal retrofit (via Waitangi Tribunal jurisprudence from the 1970s onward) imposed onto a historical event that functioned as cession in practice for over a century?',
    'Historical-linguistic and archival analysis of contemporaneous understanding (missionary records, rangatira testimony, Colonial Office correspondence) versus analysis of the doctrinal history of when and why the partnership principle was articulated in modern jurisprudence.',
    'If retrofit, the reading''s founding_problem_corroboration weakens considerably and the reading looks more like a legitimating narrative constructed after the fact; if recovery, the reading''s claim to represent the actual original bargain strengthens and the extractiveness authored here should be read as breach-of-original-terms rather than contested-interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_as_retrofit_or_recovery, empirical, 'Whether this reading recovers original intent or retrofits modern doctrine onto history.').

omega_variable(
    consent_mechanism_specificity,
    'What specific mechanism operationalizes ''ongoing consent'' in this reading — unanimous hapū agreement, majority iwi governance body agreement, or something else — and who adjudicates when consent is contested or fragmented across hapū with differing views?',
    'Examination of actual co-governance and settlement negotiation practice to see what consent threshold the Crown and Tribunal have in practice treated as sufficient.',
    'An underspecified consent mechanism makes the coordination story harder to distinguish from a Crown-controlled process dressed in partnership language; a well-specified mechanism strengthens the rope characterization of this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_mechanism_specificity, conceptual, 'Ambiguity in what counts as adequate consent under the partnership model.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 1840, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(trea_tr_t1865, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1865, 0.2).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1900, 0.35).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(trea_tr_t2025, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1840, 0.35).
narrative_ontology:measurement(trea_be_t1865, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1865, 0.72).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1900, 0.8).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1975, 0.6).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(trea_be_t2025, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1840, 0.3).
narrative_ontology:measurement(trea_su_t1865, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1865, 0.75).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(trea_su_t2025, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2025, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__rangatiratanga_retention_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(treaty_authority_cession__rangatiratanga_retention_reading, 0.12).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel treaty_authority_cession, decomposed per the ε-invariance principle: crown_cession_reading holds the English text controls and full sovereignty was ceded (likely a lower-extraction, higher-legitimacy reading from the Crown's institutional position but a snare-adjacent reading from a Māori standpoint); rangatiratanga_retention_reading (this story) holds a bounded governance cession with retained rangatiratanga and an ongoing-consent partnership, structurally a tangled rope with real coordination function marred by historical breach; retrospective_snare_exposure treats the textual divergence itself as the extraction mechanism, exposing land alienation and legislative override as operating under manufactured mistranslation ambiguity, and would be authored with substantially higher ε and a cleaner snare classification. The three do not average into one ε — each is a distinct structural claim about the same founding instrument.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__rangatiratanga_retention_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
