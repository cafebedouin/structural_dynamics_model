% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__hybrid_transformation_reading, []).

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
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Passover Seder Hybrid Memory-Rehearsal Obligation
 *   domain: religious/ritual/collective-memory
 *
 * SUMMARY:
 *   The Passover seder is a hybrid memory technology: a single annual
 *   household rite that simultaneously preserves loss-memory (bitter herbs,
 *   salt water, the plague recitation — mourning-practice in the D1/D4 sense)
 *   and rehearses adaptive capacity (scripted questioning, narrative mastery,
 *   decentralized home-based continuity — survival-competence in the D5
 *   sense). This file instantiates the HYBRID TRANSFORMATION READING of the
 *   catastrophe_memory_function kernel: the claim is that the two functions
 *   are jointly operative in one structure, not separable into distinct
 *   rites. ASSUMPTIONS: (1) the epsilon referent is the standing
 *   seder-obligation arrangement as the hybrid reading assesses it — not the
 *   abolitionist-free alternative and not either sibling's isolated function;
 *   (2) the interval runs from the post-Temple invention of the seder (70 CE)
 *   to the contemporary denominational era. CONSTRAINT FAMILY: the colloquial
 *   label 'Passover memory function' decomposes into three structurally
 *   distinct claims per the epsilon-invariance principle. The
 *   mourning_practice_reading authors lower epsilon (pure
 *   identity-coordination, participation cost only); the
 *   survival_competence_reading shifts the beneficiary seat toward future
 *   generations and reweights extraction toward institutional overhead; this
 *   hybrid reading authors moderate epsilon because the integrated structure
 *   both delivers dual goods and extracts asymmetric labor and sanction
 *   costs. Each sibling is a separate file; all three are linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - diaspora_households: primary beneficiary (organized/constrained) — receives identity cohesion and transmitted competence, bears hosting costs
 *   - seder_preparation_laborers: primary target (moderate/constrained) — bears the unnamed preparation labor the liturgy never records
 *   - rabbinic_authorities: agenda-setter and secondary beneficiary (institutional/mobile) — administers the obligation, collects standing rather than material capture
 *   - children_participants: dual-positioned (powerless/trapped) — pedagogical recipients who cannot consent and cannot leave
 *   - secular_adapters: resisting payers (organized/mobile) — modify the rite and absorb sanction
 *   - intermarried_households: excluded voice (moderate/constrained) — would contest purity enforcement, absent from adjudication
 *   - holocaust_memory_institutions: secondary beneficiary (institutional/mobile) — rides the rite's reach, supplies the added mourning layer
 *   - ritual_studies_scholars: analytical observer — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.46).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.38).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Passover Seder Hybrid Memory-Rehearsal Obligation").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious/ritual/collective-memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__hybrid_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, '000e8998-6268-4c66-932b-19d242ccdc28').
narrative_ontology:cs_kernel_codification('000e8998-6268-4c66-932b-19d242ccdc28', fixed_text).
narrative_ontology:cs_authority_grounding('000e8998-6268-4c66-932b-19d242ccdc28', lineage).
narrative_ontology:cs_interpretation_layer_present('000e8998-6268-4c66-932b-19d242ccdc28').
narrative_ontology:cs_reading_relation('000e8998-6268-4c66-932b-19d242ccdc28', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('000e8998-6268-4c66-932b-19d242ccdc28', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('000e8998-6268-4c66-932b-19d242ccdc28', foundational, ritual_functions_jointly_necessary).
narrative_ontology:cs_axiom_status(ritual_functions_jointly_necessary, holdable).
narrative_ontology:cs_axiom_grounding('000e8998-6268-4c66-932b-19d242ccdc28', ritual_functions_jointly_necessary, empirically_contingent).
narrative_ontology:cs_axiom('000e8998-6268-4c66-932b-19d242ccdc28', foundational, commemoration_requires_embodied_performance).
narrative_ontology:cs_axiom_status(commemoration_requires_embodied_performance, holdable).
narrative_ontology:cs_axiom_grounding('000e8998-6268-4c66-932b-19d242ccdc28', commemoration_requires_embodied_performance, empirically_contingent).
narrative_ontology:cs_reference_frame('000e8998-6268-4c66-932b-19d242ccdc28', integrated_grief_competence_rite).
narrative_ontology:cs_drift_state('000e8998-6268-4c66-932b-19d242ccdc28', contemporary_denominational_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('000e8998-6268-4c66-932b-19d242ccdc28', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, diaspora_households).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, holocaust_memory_institutions).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, seder_preparation_laborers).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, secular_adapters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, children_participants).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, diaspora_households).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, children_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Performs the seder annually in the home: receives identity cohesion, a structured container for catastrophic grief, and rehearsed survival-competence passed to the next generation. Bears hosting, time, and preparation costs. Skipping a year carries communal visibility cost; leaving the practice entirely means assimilation out of the community rather than a neutral exit.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, diaspora_households, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, diaspora_households, payer).

% Executes weeks of preparation the liturgy never names: kashering kitchens, purging leaven, cooking multiple ceremonial meals, cleaning. Historically and predominantly women, transmitting the role mother-to-daughter. Consumes the seder they produce, but the labor asymmetry within the household persists across denominations; reducing it requires household negotiation or movement-level liturgical change, both socially priced.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, seder_preparation_laborers, payer,
    moderate, generational, constrained, global).

% Codified the haggadah sequence, adjudicates Passover law, trains the questioners, and rules on length, language, and inclusion. Derives standing and adjudication centrality from administering the obligation. Mass-printed haggadot and denominational fragmentation ended the gatekeeping monopoly, leaving prestige and relevance rather than material capture; individual authorities can relocate between communities and their expertise travels with them.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, rabbinic_authorities, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, rabbinic_authorities, beneficiary).

% Required at the table and cast as the questioners the rite's own script needs: receive the narrative, the deprivation cues, and the survival rehearsal years before they can consent. Cannot leave the room without breaking the pedagogy the structure depends on. Their recitation typically precedes comprehension; the obligation lands on them as family presence, not negotiable commitment.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, children_participants, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, children_participants, payer).

% Run modified or secularized seders: freedom seders, feminist haggadot, shortened or vernacular scripts. Pay social sanction from kin and communal bodies for deviation. Their exit to fully secular life is real but priced — it severs the intergenerational transmission many of them still want, so they adapt inside the structure rather than leave it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, secular_adapters, payer,
    organized, biographical, mobile, continental).

% Sit outside full communal recognition in much of the organized community. Would contest purity-of-practice enforcement and claim the rite for mixed families raising Jewish children. Absent from halakhic deliberation; present only at the margins through outreach liturgies written for them by others.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, intermarried_households, excluded,
    moderate, generational, constrained, global).

% Layer Shoah commemoration onto the seder through new haggadot, added readings, and table rituals. Gain annual audience and legitimacy from the rite's built-in reach into nearly every observing household. Depend on the constraint's persistence for their commemorative channel; contribute the added mourning content themselves rather than capturing the rite's existing gains.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, holocaust_memory_institutions, beneficiary,
    institutional, generational, mobile, global).

% Analyze the seder as a memory technology: take testimony from every seat, compare haggadot across centuries, and publish accounts of which functions bind the practice. Neither collects nor pays; their classifications feed back into communal self-understanding.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, ritual_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__hybrid_transformation_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__hybrid_transformation_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves catastrophic memory and transmits adaptive capacity across generations without central infrastructure: each household annually rehearses the exodus narrative, stages scripted questions, and enacts deprivation cues (bitter herbs, salt water, unleavened bread), solving the dispersed-population continuity problem through decentralized home performance that survives the destruction of any institution.
% TRANSFER_FUNCTION: Moves preparation labor and time from household members — disproportionately the preparation laborers — into the annual rite; moves narrative authority and adjudication standing to rabbinic structures; moves identity, loss-memory, and survival-competence to children; moves commemorative reach to modern memory institutions layered onto the liturgy.
% ABSENT_VOICES: Intermarried households excluded from full recognition; the unnamed preparers the liturgy never thanks; children before the age of consent. They stand outside halakhic deliberation — in secular households, and in feminist and freedom-seder liturgies produced at the movement margins rather than inside the adjudicating bodies.
% DISAPPEARANCE_RATIONALE: If the seder obligation vanished overnight, the diaspora's most durable annual transmission node would go dark: loss-memory would migrate to museums and memorial days, household-level competence rehearsal would lapse within two to three generations, and rabbinic and memory institutions would lose their principal recurring audience. Communities would build substitutes, but the decentralized home-based architecture — the part that survived every institutional destruction — would not reassemble quickly.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE) and under successive expulsions, a dispersed people needed a way to mourn repeated catastrophes, keep covenant identity intact, and transmit survival capacity without territory, temple, or central state.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: diaspora historiography in the Zakhor tradition documents the continuity problem as ongoing rather than solved; post-Shoah survivor testimony and secular Jewish educators independently attest that memory transmission remains an open necessity; community-foundation continuity surveys, commissioned for demographic rather than ritual reasons, treat intergenerational transmission as a live problem requiring intervention.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__hybrid_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__hybrid_transformation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__hybrid_transformation_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).
:- end_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.46 at interval end): the arrangement extracts real, asymmetrically distributed costs — weeks of unnamed preparation labor, required child participation, sanction-priced deviation — while delivering substantial, broadly shared goods, so epsilon sits well below snare range and well above pure-coordination floor. Suppression (0.38) is a raw structural property, NOT scaled by power or scope: it reflects communal sanction, marriage-market effects, and the social price of skipping, not violent enforcement; exits exist but are priced. Theater ratio (0.27) is low-moderate: most of the rite is functional transmission, but a growing share of contemporary performance is recitation without comprehension — an ambiguity routed to the rote_recitation_function omega rather than resolved by fiat. Accessibility collapse (0.40) is low: alternatives (secular seders, feminist haggadot, skipped years) remain workable at social cost. Resistance (0.42) is real and productive: emancipation-era reform, feminist liturgy, and freedom seders changed the rite without destroying it. The measurement series run on ONE shared time grid (70, 200, 500, 1000, 1500, 1800, 1945, 2025) with every tracked metric authored at every point. The suppression_requirement series is authored deliberately: it traces enforcement-capacity change (consolidation through the medieval kehillah era, decline after emancipation), not merely extraction shift. The 1945 inflection is the key event: the Shoah reactivated the mourning function, briefly lowering theater and raising extractiveness as remembrance became a sacred duty; the subsequent decline models liberalization and adaptation. The dynamics are not cyclical; they are monotonic arcs with one exogenous shock.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute differently. From the rabbinic seat the arrangement is a faithfully transmitted dual-function rite the community would be lost without; from the preparation-laborer seat it is an annual extraction of weeks of unrecognized work justified by a liturgy that never mentions the cook; from the secular-adapter seat it is a boundary mechanism that punishes precisely the modifications that keep it livable. Children occupy the sharpest divergence: the rite's own script casts them as beneficiaries of transmission while binding them before consent. Identity-lock is engineered here, not incidental: 'in every generation one must see oneself as having come out of Egypt' fuses participant identity with ancestral experience — an ideological-relational fusion mechanism. If that frame broke, exit would become thinkable as ordinary assimilation rather than betrayal; the engine should expect the trapped seats (children, laborers) to classify more extractively than the mobile seat (rabbis) from identical structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Diaspora households are net beneficiaries (low d): the coordination good dominates their hosting cost, though their secondary payer position keeps them off the pure-beneficiary pole. Seder preparation laborers are the clearest targets (high d): they bear concentrated, unnamed, gendered cost through the same structure that delivers the good. Secular adapters are targets whose mobility tempers derived d — they can leave, but the exit severs something they value, so they are not arbitrage-grade. Rabbinic authorities derive near the beneficiary pole: they administer and collect standing, and their mobile exit plus institutional power places them far from extraction. Children derive mid-to-low d: heavy receipt of the transmission good against a compliance cost they cannot refuse. Holocaust memory institutions are beneficiaries of the rite's reach but supply their own added content. No directionality overrides were authored: the declared roles plus exit options already produce the correct ordering, and the override mechanism keys on power atom, which would smear the distinction between laborers and adapters who share a power level but not a relationship to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how a dispersed people preserves memory and adaptive capacity without central infrastructure — is live, corroborated from outside the beneficiary set, so no mandatrophy is declared and the R5 mismatch consumer finds status=live x verdict=world_rearranges (no zombie flag). The tangled_rope classification is what prevents mislabeling in both directions: a pure-rope reading would erase the labor asymmetry and the sanction structure (treating 'everyone benefits' as the whole truth); a snare reading would erase the demonstrated dual delivery — the rite measurably preserved a stateless people's identity and adaptive repertoire for two millennia, which no pure extraction does. The receipt-surface findings reinforce the hybrid verdict: gain_flow is affirmatively 'diffuse' (each named seat was checked — material extraction recirculates within the producing households, rabbinic capture collapsed to prestige after print culture broke the gatekeeping monopoly, and memory institutions supply the content they benefit from), and fixing_cost is 'cheap': liberal movements demonstrated that the extractive edges (labor asymmetry, length, exclusion) are reformable without collapsing the transmission function, which is exactly the signature of a live tangled_rope rather than a captured snare or an inert piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Does the hybrid framing capture the seder''s operative structure, or does one sibling function — mourning-preservation or competence-transmission — alone bind the arrangement''s persistence?',
    'Comparative ritual observation: measure persistence and fidelity in communities that strip one function (festive seders without mourning segments; memorial seders without rehearsal pedagogy) and track which stripped function predicts abandonment across generations.',
    'If mourning-preservation alone binds, this reading collapses toward the mourning sibling with lower epsilon and fewer victim seats; if competence-transmission binds, the beneficiary set shifts toward future-generation seats and the extraction profile reweights toward institutional overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Whether the hybrid reading or a sibling isolates the binding constraint of the catastrophe_memory_function kernel.').

omega_variable(
    preparation_labor_visibility,
    'How much unpaid preparation labor does the arrangement actually extract, given that the textual record systematically under-documents domestic work?',
    'Time-use studies of seder-observing households, disaggregated by gender and denominational community, compared against the liturgical record.',
    'Higher measured labor raises epsilon and strengthens the payer seat of the preparation laborers; near-parity would move the arrangement toward rope and weaken the tangled_rope gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preparation_labor_visibility, empirical, 'Whether the epsilon estimate correctly counts the invisible domestic labor component.').

omega_variable(
    rote_recitation_function,
    'Is low-comprehension recitation theatrical decay (proxy replacing function) or developmentally staged pedagogy in which text legitimately precedes meaning?',
    'Longitudinal tracking of participants who recited before comprehending: does adult meaning-engagement correlate with childhood rote exposure, or does rote exposure predict disengagement?',
    'If pedagogical, the theater_ratio is overstated and the arrangement is healthier than scored; if decay, theater is understated and piton drift risk rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rote_recitation_function, empirical, 'Whether contemporary recitation-without-comprehension is Goodhart drift or staged learning.').

omega_variable(
    shoah_layer_direction,
    'Did the post-Shoah layers reactivate the integrated mourning function, or convert the seder toward a general memorial vehicle aligned with the mourning-only sibling reading?',
    'Textual and observational analysis of post-1945 haggadot and lived seder scripts: is Shoah memory integrated with the exodus rehearsal or substituted for it?',
    'Substitution would shift this story''s epsilon toward the mourning sibling''s profile and date a partial reading-drift within the kernel family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shoah_layer_direction, empirical, 'Direction of the post-1945 commemorative layering relative to the hybrid structure.').

omega_variable(
    obligation_internalization,
    'Is the measured suppression structural (communal sanction, marriage-market effects, visibility cost) or internalized (obligation experienced as self-evident duty)?',
    'Post-exit trajectory study of assimilated households: if obligation-feeling persists after sanction exposure ends, internalization is substantial.',
    'Internalized suppression raises effective suppression above the structural measure and supports an identity_locked reading of household exit that the current constrained rating does not capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obligation_internalization, empirical, 'Structural versus internalized suppression mechanism in the communal obligation.').

omega_variable(
    authority_fragmentation,
    'Is seder authority still lineage-grounded with a functioning interpretive layer, or effectively distributed across denominations with no designated interpreter?',
    'Map adjudication in practice: when disputes arise over length, language, or inclusion, determine which body''s ruling binds whom, and whether any ruling binds beyond its own denomination.',
    'A distributed-authority finding would change the cs_structure classification, weaken the agenda_setter seat of the rabbinic authorities, and raise the weight of household-level autonomy in the epsilon computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_fragmentation, conceptual, 'CS-framing under-determination: lineage-with-interpreter versus de facto distributed authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 70, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t70, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 70, 0.1).
narrative_ontology:measurement(cata_tr_t200, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 200, 0.12).
narrative_ontology:measurement(cata_tr_t500, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 500, 0.15).
narrative_ontology:measurement(cata_tr_t1000, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 1000, 0.18).
narrative_ontology:measurement(cata_tr_t1500, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(cata_tr_t1800, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 1800, 0.28).
narrative_ontology:measurement(cata_tr_t1945, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 1945, 0.22).
narrative_ontology:measurement(cata_tr_t2025, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 2025, 0.27).

% Extraction over time
narrative_ontology:measurement(cata_be_t70, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 70, 0.38).
narrative_ontology:measurement(cata_be_t200, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 200, 0.44).
narrative_ontology:measurement(cata_be_t500, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 500, 0.47).
narrative_ontology:measurement(cata_be_t1000, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 1000, 0.5).
narrative_ontology:measurement(cata_be_t1500, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 1500, 0.52).
narrative_ontology:measurement(cata_be_t1800, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 1800, 0.49).
narrative_ontology:measurement(cata_be_t1945, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 1945, 0.53).
narrative_ontology:measurement(cata_be_t2025, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 2025, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t70, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 70, 0.3).
narrative_ontology:measurement(cata_su_t200, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 200, 0.42).
narrative_ontology:measurement(cata_su_t500, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 500, 0.5).
narrative_ontology:measurement(cata_su_t1000, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 1000, 0.58).
narrative_ontology:measurement(cata_su_t1500, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 1500, 0.6).
narrative_ontology:measurement(cata_su_t1800, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 1800, 0.48).
narrative_ontology:measurement(cata_su_t1945, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 1945, 0.44).
narrative_ontology:measurement(cata_su_t2025, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__survival_competence_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the kernel 'catastrophe_memory_function'. The colloquial label conflates three structurally distinct claims: (1) the mourning_practice_reading — the rite preserves memorial obligation and boundary norms (D1/D4), authors lower epsilon, pure identity-coordination shape; (2) the survival_competence_reading — the rite transmits adaptive capacity for decentralized continuity (D5), shifts the beneficiary seat toward future generations; (3) this hybrid_transformation_reading — both functions are jointly operative in one structure, authors moderate epsilon with named victim seats. The hybrid reading is downstream of both siblings in argument structure: it cites each partial reading as evidence that its function is real, then claims joint necessity. All three files link one another via affects_constraints; epsilon differs across the family because each reading assesses the same standing arrangement through a different functional lens, not because the arrangement differs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
