% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__crown_cession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__crown_cession_reading, []).

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
 *   constraint_id: treaty_authority_cession__crown_cession_reading
 *   human_readable: Crown Cession Reading of Treaty Authority
 *   domain: constitutional/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) is a written accord between British
 *   colonial authority and Māori chiefs governing sovereignty, rights, and
 *   governance in New Zealand. This constraint story instantiates the CROWN
 *   CESSION READING: the interpretation that the English text controls, that
 *   'kāwanatanga' (the Māori word for governance) means full sovereignty
 *   ceded to the Crown, that Māori customary authority (rangatiratanga) is
 *   subordinate, and that the Treaty completed a legal transfer of ultimate
 *   authority from Māori to the Crown. Under this reading, the cession is
 *   presented as a coordination device solving the problem of unified
 *   governance; the metrics reveal it as substantially extractive, actively
 *   enforced against Māori objections, and increasingly defended through
 *   performative rather than functional means. The constraint is CLAIMED as
 *   tangled_rope (coordination + extraction coexist) to capture the reading's
 *   genuine coordination element (unified legal order) alongside its
 *   extractive operation (transfer of authority without consent to
 *   equivalents). The measurement series tracks a long interval (180 units ≈
 *   generations, 1840–2020) showing initial very high extraction declining
 *   slightly through mid-20th century (period of Crown governance efficiency
 *   and settler consolidation), rising theater ratio from mid-20th century
 *   onward (as Māori resistance grew and the Crown invested in legitimation
 *   narratives and treaty settlements), and extraction recovering to high
 *   levels in contemporary period (as the cession reading faces renewed
 *   challenge). The theater ratio's rise and recovery reflects the constraint
 *   shifting from straightforward enforcement to narrative defense, a
 *   piton-characteristic sign.
 *
 * KEY AGENTS:
 *   - British Crown (institutional agenda-setter; collects sovereignty, land-control authority)
 *   - Settler colonists (organized beneficiary; gains secure property title and polity membership)
 *   - Māori iwi (powerless payers; lose territorial authority, identity-locked to the legal system)
 *   - Treaty interpretation courts (institutional agenda-setter; operationalizes the cession reading through doctrine)
 *   - Māori political movements (excluded; would contest but are outside the authoritative interpretation frame)
 *   - Māori landholders (powerless trapped victims; lose control of ancestral lands)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.82).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.71).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, tangled_rope).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Crown Cession Reading of Treaty Authority").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, 'd9721ea3-721b-4cd8-a6df-68d8dda328fc').
narrative_ontology:cs_kernel_codification('d9721ea3-721b-4cd8-a6df-68d8dda328fc', fixed_text).
narrative_ontology:cs_authority_grounding('d9721ea3-721b-4cd8-a6df-68d8dda328fc', extraction).
narrative_ontology:cs_interpretation_layer_present('d9721ea3-721b-4cd8-a6df-68d8dda328fc').
narrative_ontology:cs_reading_relation('d9721ea3-721b-4cd8-a6df-68d8dda328fc', treaty_authority_cession__rangatiratanga_retention_reading, coexists_with).
narrative_ontology:cs_reading_relation('d9721ea3-721b-4cd8-a6df-68d8dda328fc', treaty_authority_cession__retrospective_snare_exposure, influences).
narrative_ontology:cs_axiom('d9721ea3-721b-4cd8-a6df-68d8dda328fc', foundational, english_text_controls_meaning).
narrative_ontology:cs_axiom_status(english_text_controls_meaning, holdable).
narrative_ontology:cs_axiom_grounding('d9721ea3-721b-4cd8-a6df-68d8dda328fc', english_text_controls_meaning, conventional).
narrative_ontology:cs_axiom('d9721ea3-721b-4cd8-a6df-68d8dda328fc', foundational, kawanatanga_equals_full_sovereignty_cession).
narrative_ontology:cs_axiom_status(kawanatanga_equals_full_sovereignty_cession, holdable).
narrative_ontology:cs_axiom_grounding('d9721ea3-721b-4cd8-a6df-68d8dda328fc', kawanatanga_equals_full_sovereignty_cession, empirically_contingent).
narrative_ontology:cs_reference_frame('d9721ea3-721b-4cd8-a6df-68d8dda328fc', crown_sovereign_authority_established).
narrative_ontology:cs_drift_state('d9721ea3-721b-4cd8-a6df-68d8dda328fc', contemporary_treaty_settlement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d9721ea3-721b-4cd8-a6df-68d8dda328fc', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__crown_cession_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, british_crown).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, settler_colonists).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_iwi).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, treaty_interpretation_courts).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_landholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The British Crown and its agents (courts, government) interpret and enforce the cession reading. They claim the Treaty transferred full sovereignty to the Crown, giving it authority to make law, alienate land, and govern the territory. They justify this as necessary for coherent governance and settler security. They benefit by collecting sovereign authority and the ability to legitimize land transfers to settler colonists.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, british_crown, agenda_setter,
    institutional, generational, analytical, global).

% Settler colonists (European and later waves of migrants) benefit from the cession reading because it legitimizes their acquisition of Māori land through Crown-issued title, gives them full membership in the polity and its governance institutions, and protects their property against Māori claims. Without the cession reading, their land tenure and political standing would rest on contested grounds.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, settler_colonists, beneficiary,
    organized, generational, arbitrage, national).

% Māori iwi (tribes) are constructed as payers under this reading. They sign the Treaty and are told they have ceded sovereignty to the Crown. Their customary authority ('rangatiratanga') is interpreted as subordinate. They lose the ability to make law, control land, and govern their territories. They cannot exit the system without ceasing to exist as a territorial people—they are identity-locked to the colonial legal framework.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_iwi, payer,
    powerless, generational, identity_locked, national).

% New Zealand courts (starting with the Supreme Court, later the Court of Appeals and Waitangi Tribunal) interpret what the Treaty means. Under the cession reading, they apply English-text primacy, read kāwanatanga as full sovereignty, and interpret Māori objections within the Crown's established authority framework. The courts benefit because this reading stabilizes the legal order and makes their authority over the interpretation uncontestable.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, treaty_interpretation_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__crown_cession_reading, treaty_interpretation_courts, beneficiary).

% Māori political and intellectual movements (starting with organized resistance in the 1970s) contest the cession reading and advocate for alternative interpretations (rangatiratanga retention, partnership models, snare exposure via textual analysis). They are excluded from the authoritative interpretation machinery—their objections are heard as political advocacy rather than legal argument with equal standing. They are constrained by the institutional gatekeeping that operationalizes the cession reading.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_political_movements, excluded,
    moderate, generational, constrained, national).

% Individual Māori landholders (descendants of chiefs and people who held land at 1840) lose effective control of their ancestral lands through alienation processes the Crown's sovereignty claim legitimizes. They face land confiscation, forced sales, and Crown appropriation justified by the Crown's claimed authority. They cannot exit without ceasing to exist as a landowning people.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_landholders, payer,
    powerless, biographical, trapped, local).

% The doctrine that English text controls interpretation of treaties with indigenous peoples, applied over indigenous-language texts, operates as a vindicated proposition under the cession reading. This principle is not a real actor but an institutional idea that the reading depends on and that benefits from the cession reading's dominance.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, english_text_authority, beneficiary,
    powerful, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(treaty_authority_cession__crown_cession_reading, english_text_authority).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__crown_cession_reading, british_crown).
narrative_ontology:fixing_cost_class(treaty_authority_cession__crown_cession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single unified legal order with Crown sovereign authority at the apex, enabling coherent law-making, property registration, and commerce across the territory. Replaces a prior condition of contested jurisdictions and competing authorities with settled Crown governance.
% TRANSFER_FUNCTION: Moves ultimate authority over land, law-making, and territorial governance from Māori iwi to the British Crown and its settler representatives. In material terms: transfers control of land alienation, law-making power, and resource extraction rights from Māori to Crown-aligned actors.
% ABSENT_VOICES: Māori political movements, indigenous scholars, and alternative legal traditions that read the Māori text differently are structurally excluded from the authoritative interpretation table. They would testify that the cession reading misrepresents the chiefs' intent and operates as enforced extraction rather than coordination. Rival interpretations of the same Treaty document (rangatiratanga retention, partnership models) are not heard as equal framings but as political challenges to settled law.
% DISAPPEARANCE_RATIONALE: If the cession reading disappeared and was replaced by a reading that retained Māori territorial authority and governance rights, the entire legal foundation of Crown sovereignty, land title, and settler property would destabilize. The colonial state itself is built on the cession reading; its disappearance would require redrawing the boundaries of legitimate authority.
% FOUNDING_PROBLEM: Prior to the Treaty, British interests in New Zealand faced competing claims to territory and no unified legal authority to guarantee settler security or land transactions. The cession reading was constructed to solve this: by interpreting the Treaty as transferring sovereignty to the Crown, it created a single authoritative source of law and property legitimacy that settlers could rely on.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (lack of unified Crown authority) was real in 1840. But by the 1970s-2000s, Māori scholars and the New Zealand Court of Appeals began testifying that the founding problem was solved through Crown governance itself—the coordination benefit was achieved—and the continued reading of the Treaty as a cession was sustaining extraction without coordination justification. The government's own treaty claims process (starting 1975) implicitly acknowledged this: if the cession was complete and valid, there would be no 'claims' to settle. The foundational problem's status is contested: Crown authorities treat it as permanently live (sovereignty must be defended); Māori movements treat it as dead (sovereignty is established; the cession reading now extracts without solving the original coordination problem).
narrative_ontology:disappearance_verdict(treaty_authority_cession__crown_cession_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__crown_cession_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__crown_cession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(treaty_authority_cession__crown_cession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__crown_cession_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__crown_cession_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__crown_cession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.82) reflects the reading's operation: it transfers vast authority (land control, law-making) from Māori to Crown with no equivalent transfer backward. Suppression (0.71) is high because the constraint's persistence depends on actively suppressing rival readings and constraining Māori objection-making within Crown-defined procedures (the treaty claims process itself is structured so Māori file claims to the Crown for redress of Crown wrongs—a procedural suppression). Theater ratio (0.38 at interval end) climbs through the interval, signaling that from mid-20th century forward, more of the constraint's maintenance work goes into legitimation narratives (public discourse about partnership, inclusion, 'honouring the treaty') than into raw enforcement. Accessibility collapse (0.68) is high because the cession reading, once institutionalized in courts and doctrine, constrains alternative readings—to reject cession within the legal system is to reject the entire legal order. Resistance (0.45) is moderate, not high: while Māori movements actively resist, the powerless agent pool (iwi, landholders) has limited capacity to mount organized resistance against an institutional agenda-setter. The architecture is tangled_rope, not pure snare, because the Crown cession reading genuinely does solve the coordination problem it claims to solve (unified law-making, settled property, coherent governance). But it solves it through extraction—the coordination could have been achieved through partnership frameworks (the rival rangatiratanga reading) that would not require Māori authority transfer. The reading's extractiveness lies in using the coordination problem to justify the authority transfer, not in the coordination itself.
 *
 * PERSPECTIVAL GAP:
 *   The Crown and courts compute this constraint as legitimate coordination (sovereign authority necessary for law-making and security). Māori iwi compute it as extraction disguised as coordination (the authority transfer was unnecessary for unified governance and served settler interests exclusively). The engine computes different directionality values for these seats: the Crown approaches d≈0.0 (beneficiary of the arrangement, controls its terms, set the agenda), while Māori iwi approach d≈1.0 (targets of extraction, constrained exit, subordinated authority). The theater ratio's rise indicates that by the contemporary period, the Crown increasingly deploys legitimation narratives (treaty settlements, partnership language, inclusion frameworks) to defend an arrangement whose functional coordination problem (unified governance) has long been solved. This shift is the sign of extraction without current coordination justification—the reading persists by theater, not by the original solving-power.
 *
 * DIRECTIONALITY LOGIC:
 *   The British Crown is the structural beneficiary: it collects sovereignty, controls law-making, and gains the authority to alienate Māori land. Its exit options are analytical (as an institutional actor, it exits through abdication of sovereignty, not through the cost-driven exit logic powerless agents experience). Settler colonists are beneficiaries with organized power: they gain secure property title and polity membership, stakes the Crown's sovereignty claim makes possible. Their exit would require abandoning New Zealand entirely—a mobile option in principle (arbitrage), but one they rarely take. Māori iwi are the targets: they lose authority, face land alienation, and cannot exit the system without ceasing to exist as territorial peoples (identity_locked). Courts are institutional agents; they benefit from the interpretive closure the cession reading provides, which makes their authority uncontestable. Māori landholders are trapped (cannot exit without becoming stateless or landless). The directionality asymmetry is the constraint's core structure: high-power institutional agents (Crown, courts) benefit and set terms; powerless agents (iwi, landholders) pay and have no exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unified Crown governance without a legitimate legal framework) was real in 1840. By the 1900s it was solved: the Crown governed effectively, settler property was secure, law was coherent. But the cession reading persists. The 1975 treaty claims process acknowledges this paradox implicitly: if the cession was legitimate and complete, there would be nothing to claim—the fact that the Crown acknowledged a claims process means it implicitly recognized the founding problem was solved and the reading's original justification was exhausted. From ~1980 onward, the Crown invests heavily in treaty settlements and partnership language, signs of theater ratio rising. The constraint shows mandatrophy: the founding coordination problem is dead (Crown governance is established and functioning), but the reading persists, defended increasingly through narrative legitimation rather than functional necessity. The contemporaneous measurement point (t=180) shows a recovery of extractiveness to near its starting level (0.82), suggesting renewed contestation and the Crown's reassertion of the cession reading against Māori resurgence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    english_text_primacy_doctrine,
    'Is English text primacy a natural application of treaty interpretation law, or a doctrine chosen to advantage one party''s interests?',
    'Comparative analysis of how other colonial treaties with indigenous peoples are interpreted—do courts consistently apply English-text primacy, or only when it favors settler interests? Do courts apply contra proferentem (ambiguity against the drafter) in other contexts? Legal history analysis of how the doctrine was established in New Zealand courts.',
    'If text primacy is a natural interpretive rule, the cession reading reflects sound jurisprudence. If it was chosen to advantage the Crown, the reading''s legitimacy is partly constructed rather than legally grounded. This is the core uncertainty about whether the cession reading is robust law or extractive doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(english_text_primacy_doctrine, empirical, 'Whether English text primacy is a neutral interpretive principle or a chosen doctrine favoring Crown interests').

omega_variable(
    kawanatanga_meaning_stability,
    'Did ''kāwanatanga'' carry a meaning the Māori signatories understood as full sovereignty cession, or a narrower meaning of governance right delegated with retained ultimate authority?',
    'Historical linguistic analysis of kāwanatanga usage in Māori texts before 1840, analysis of what Māori chiefs understood by the term (oral histories, contemporary Māori accounts), examination of the Crown''s own pre-treaty statements about what authority it sought.',
    'If kāwanatanga meant ''full sovereignty'' to the Māori signatories, the cession reading reflects actual consent. If it meant ''delegated governance'' or ''Crown authority in certain domains'', the cession reading misrepresents the chiefs'' understanding and becomes a retrospective extraction through interpretive reframing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kawanatanga_meaning_stability, empirical, 'Whether Māori signatories understood kāwanatanga as full sovereignty cession or delegated governance').

omega_variable(
    suppression_structural_vs_procedural,
    'Is the suppression (0.71) of rival readings primarily structural (the courts have institutional power to exclude alternatives) or internalized (Māori legal advocates have internalized the cession reading''s dominance and constrain their own objections)?',
    'Post-suppression trajectory: in jurisdictions that formally recognize alternative readings (like Canadian courts recognizing indigenous law), do suppressed readings re-emerge, or has suppression become internalized? Do Māori legal scholars outside Crown-controlled settings adopt different frames?',
    'If structural, the suppression persists as long as institutional gatekeepers defend the cession reading; opening the interpretation to alternative frameworks would immediately surface rival readings. If internalized, Māori legal advocates carry the suppression with them even in open forums, and breaking it requires decolonization work beyond institutional change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_procedural, empirical, 'Whether suppression of rival readings is institutional gatekeeping or internalized constraint').

omega_variable(
    theater_ratio_growth_driver,
    'Does the rising theater ratio from mid-20th century onward reflect the constraint''s function becoming more theatrical (extraction persisting through narrative legitimation), or Māori resistance forcing the Crown to invest more in legitimation?',
    'Comparative analysis of Crown investment in treaty narratives, settlement discourse, and partnership language before vs. after major Māori political movements gained organizational capacity (roughly 1970s onward). Did Crown expenditure on legitimation rise in response to resistance?',
    'If the theater ratio rises because the Crown invested more in legitimation against rising resistance, the constraint shifted from straightforward enforcement to contested legitimacy—a sign of weakening control. If it reflects the constraint''s functional need to defend itself (piton dynamics), it signals the founding coordination problem is dead and the reading persists by performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_growth_driver, empirical, 'Whether rising theater reflects Crown response to resistance or the constraint''s functional atrophy').

omega_variable(
    rival_reading_foreclosure_test,
    'Does the cession reading logically foreclose the rangatiratanga retention reading, or can a single legal framework hold both?',
    'Examination of whether the two readings rest on contradictory core premises (if both applied, would the same legal question have two incompatible answers?) or whether they can coexist as different interpretations of the same text (one strict, one generous; one literal, one purposive).',
    'If they foreclose each other (mutually exclusive), the readings represent genuine alternatives to the kernel and the engine would compute foreclosure. If they coexist (as different judicial approaches to the same text), both remain live and neither''s dominance is logically necessary—dominance is purely institutional/political. This determines the nature of the contest: epistemic incompatibility vs. institutional power asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rival_reading_foreclosure_test, conceptual, 'Whether the cession and rangatiratanga readings logically foreclose each other or coexist as alternative interpretations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 0, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t0, treaty_authority_cession__crown_cession_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(trea_tr_t20, treaty_authority_cession__crown_cession_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(trea_tr_t40, treaty_authority_cession__crown_cession_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(trea_tr_t60, treaty_authority_cession__crown_cession_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement(trea_tr_t100, treaty_authority_cession__crown_cession_reading, theater_ratio, 100, 0.38).
narrative_ontology:measurement(trea_tr_t140, treaty_authority_cession__crown_cession_reading, theater_ratio, 140, 0.42).
narrative_ontology:measurement(trea_tr_t180, treaty_authority_cession__crown_cession_reading, theater_ratio, 180, 0.38).

% Extraction over time
narrative_ontology:measurement(trea_be_t0, treaty_authority_cession__crown_cession_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(trea_be_t20, treaty_authority_cession__crown_cession_reading, base_extractiveness, 20, 0.85).
narrative_ontology:measurement(trea_be_t40, treaty_authority_cession__crown_cession_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(trea_be_t60, treaty_authority_cession__crown_cession_reading, base_extractiveness, 60, 0.82).
narrative_ontology:measurement(trea_be_t100, treaty_authority_cession__crown_cession_reading, base_extractiveness, 100, 0.78).
narrative_ontology:measurement(trea_be_t140, treaty_authority_cession__crown_cession_reading, base_extractiveness, 140, 0.74).
narrative_ontology:measurement(trea_be_t180, treaty_authority_cession__crown_cession_reading, base_extractiveness, 180, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t0, treaty_authority_cession__crown_cession_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(trea_su_t20, treaty_authority_cession__crown_cession_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(trea_su_t40, treaty_authority_cession__crown_cession_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(trea_su_t60, treaty_authority_cession__crown_cession_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(trea_su_t100, treaty_authority_cession__crown_cession_reading, suppression_requirement, 100, 0.68).
narrative_ontology:measurement(trea_su_t140, treaty_authority_cession__crown_cession_reading, suppression_requirement, 140, 0.65).
narrative_ontology:measurement(trea_su_t180, treaty_authority_cession__crown_cession_reading, suppression_requirement, 180, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__crown_cession_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(treaty_authority_cession__crown_cession_reading, 0.12).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'treaty_authority_cession'. The crown cession reading interprets the Treaty as completing a legal transfer of sovereignty from Māori to the Crown, reading 'kāwanatanga' as full sovereignty cession. The rangatiratanga retention reading interprets it as establishing partnership with retained Māori authority ('tino rangatiratanga'), reading the Māori text as controlling and 'kāwanatanga' as limited to governance delegation. The retrospective snare exposure reading treats the textual divergence itself as the extraction mechanism—chiefs signing Māori text could not assent to English sovereignty claims, making the cession operate as extraction by mistranslation. These three readings are not different angles on one constraint but distinct instantiations of the same kernel with different ε values and structural properties. The cession reading (this file) has ε=0.82 (substantially extractive); the rangatiratanga reading has ε much lower (cooperation-dominated); the snare reading has ε very high (pure extraction by mechanism). Network linkage via affects_constraints enables the corpus to model how different readings of the same kernel generate constraints with divergent classification properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__crown_cession_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
