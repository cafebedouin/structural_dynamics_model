% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__institutional_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__institutional_pragmatism_reading, []).

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
 *   constraint_id: plural_marriage_mandate__institutional_pragmatism_reading
 *   human_readable: 1890 Manifesto: Plural Marriage Mandate Suspension (Institutional Pragmatism Reading)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   The 1890 Manifesto, issued by the Church of Jesus Christ of Latter-day
 *   Saints, suspended the institutional practice of plural marriage in
 *   response to sustained federal coercion (asset seizure,
 *   disenfranchisement, imprisonment of polygamist leaders). The
 *   institutional pragmatism reading interprets this suspension as strategic
 *   capitulation: the leadership faced institutional dissolution and chose
 *   instead to author a narrative of prophetic revelation to legitimate the
 *   abandonment of what had been presented as a divinely mandated practice.
 *   The constraint operates at the level of institutional authority and
 *   doctrinal legitimation: it uses a reframing narrative (the Manifesto as
 *   revelation) to transform what was legally and economically forced
 *   compliance into what appears to members as obedience to divine
 *   instruction. The M-set gap (doctrine officially unchanged, practice
 *   officially suspended, secret continuations documented 1890-1904) becomes
 *   the primary observable that distinguishes pragmatism from
 *   reinterpretation. The leadership benefits from restored political
 *   standing and property rights; coerced polygamists and deceived
 *   monogamists bear costs (dissolution of families, epistemic betrayal,
 *   identity disruption). This reading does NOT claim the doctrine was never
 *   genuine or that the leadership was conscious hypocrites—it claims the
 *   constraint's persistence and form depend on the reframing narrative, and
 *   that narrative is the mechanism by which institutional survival is
 *   achieved. The claimed type is tangled_rope: genuine institutional
 *   coordination function (preserving organizational coherence under coercive
 *   pressure) entangled with asymmetric extraction (leadership gains restored
 *   rights; members lose relational integrity and doctrinal coherence).
 *
 * KEY AGENTS:
 *   - church_institutional_leadership: Designs and implements the reframing narrative; benefits from restored political rights and property. Institutional power, constrained exit (capitulation is the alternative), generational time horizon.
 *   - coerced_polygamists: Identity-locked in the faith community; bear costs of enforced family dissolution. Powerless structural position, identity-locked exit, biographical time horizon.
 *   - deceived_monogamists: Bear epistemic and relational costs of doctrinal reversal; identity-locked membership. Moderate power (as believing members, some influence), identity-locked exit, biographical time horizon.
 *   - federal_government: External force applying coercive pressure; achieves its objective (statehood, legal compliance) regardless of the narrative frame. Institutional power, arbitrage exit, national scope.
 *   - rank_and_file_members: Excluded from leadership deliberation; subject to the constraint through institutional obedience. Powerless position, identity-locked exit, biographical time horizon, regional scope.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.78).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.81).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "1890 Manifesto: Plural Marriage Mandate Suspension (Institutional Pragmatism Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, 'a8c024db-842c-4719-8dc8-56c41745e1c6').
narrative_ontology:cs_kernel_codification('a8c024db-842c-4719-8dc8-56c41745e1c6', fixed_text).
narrative_ontology:cs_authority_grounding('a8c024db-842c-4719-8dc8-56c41745e1c6', extraction).
narrative_ontology:cs_interpretation_layer_present('a8c024db-842c-4719-8dc8-56c41745e1c6').
narrative_ontology:cs_reading_relation('a8c024db-842c-4719-8dc8-56c41745e1c6', plural_marriage_mandate__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('a8c024db-842c-4719-8dc8-56c41745e1c6', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('a8c024db-842c-4719-8dc8-56c41745e1c6', foundational, doctrinal_reframing_as_institutional_survival_mechanism).
narrative_ontology:cs_axiom_status(doctrinal_reframing_as_institutional_survival_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('a8c024db-842c-4719-8dc8-56c41745e1c6', doctrinal_reframing_as_institutional_survival_mechanism, instrumental).
narrative_ontology:cs_axiom('a8c024db-842c-4719-8dc8-56c41745e1c6', foundational, prophetic_authority_serves_coercive_accommodation).
narrative_ontology:cs_axiom_status(prophetic_authority_serves_coercive_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('a8c024db-842c-4719-8dc8-56c41745e1c6', prophetic_authority_serves_coercive_accommodation, empirically_contingent).
narrative_ontology:cs_reference_frame('a8c024db-842c-4719-8dc8-56c41745e1c6', plural_marriage_divinely_mandated_authority).
narrative_ontology:cs_drift_state('a8c024db-842c-4719-8dc8-56c41745e1c6', post_1890_manifesto_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a8c024db-842c-4719-8dc8-56c41745e1c6', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, church_institutional_leadership).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__institutional_pragmatism_reading, institutional_survival_doctrine).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__institutional_pragmatism_reading, prophetic_revelation_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the official interpretive authority over scriptural mandate and designs the 1890 Manifesto as a strategic response to federal coercion (asset seizure, imprisonment of polygamist leaders, removal of church voting privileges). The leadership faces a binary: institutional dissolution or doctrinal suspension. They author the Manifesto framing suspension as prophetic revelation, legitimating capitulation by renarrating it as divine instruction rather than coerced abandonment. They benefit from restored political rights, resumed land holdings, statehood admission, and maintained organizational coherence—benefits that accrue only to the institution, not to individual members.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, church_institutional_leadership, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__institutional_pragmatism_reading, church_institutional_leadership, beneficiary).

% Had entered into plural marriages under the explicit doctrinal mandate. The 1890 Manifesto suspends that mandate and creates institutional pressure (social excommunication, leadership condemnation, family rupture) to dissolve existing plural families. They bear the cost of doctrinal reversal on their most intimate relationships. Their identity as covenantal members and as polygamists is fused—exit from polygamy means exit from the faith community itself or permanent marginal status. The institutional survival that the Manifesto secures does not restore their polygamist status or rights; it extinguishes the doctrinal foundation that once legitimated their marriages.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamists, payer,
    powerless, biographical, identity_locked, regional).

% Were taught and believed that plural marriage was a divine requirement. The 1890 Manifesto frames its abandonment as prophetic correction, implicitly reinterpreting decades of teaching as temporary or as subject to divine revision unknown to members. They bear the epistemic and relational costs: their understanding of divine will has been reversed; families that delayed plural marriage on doctrinal grounds learn the doctrinal basis has shifted; trust in institutional teaching authority is damaged. Their identity as believing members is fused with faith in the institution's doctrine—they cannot easily exit without losing their entire social and spiritual world.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists, payer,
    moderate, biographical, identity_locked, regional).

% Applies coercive pressure (criminal statutes, asset seizure, disenfranchisement) to force abandonment of plural marriage. The federal government does not participate in the institutional structure of the constraint itself—it is the external force that makes the constraint's particular form (doctrinal reframing rather than simple prohibition) necessary. The government's interest is statehood admission (Utah) and national legal uniformity; it achieves its objective when the institutional leadership capitulates, regardless of the narrative frame used.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Are subject to the constraint but hold no seat in the decision that authored it. They learn of the Manifesto through leadership announcement, not through consultation. Their role in the institutional structure is obedience to leadership direction. Secret continuations of plural marriage (documented 1890-1904) occur beyond the formal membership base and create a parallel, concealed institutional reality that ordinary members are neither party to nor informed of.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, rank_and_file_members, excluded,
    powerless, biographical, identity_locked, regional).

% Examine the historical record and competing framings of what the 1890 Manifesto represents: institutional pragmatism masquerading as divine revelation, or legitimate doctrinal reinterpretation, or federal coercion succeeding where doctrine was never genuine. They hold no stake in the constraint's operation but provide evidentiary grounding for competing readings.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, academic_observers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__institutional_pragmatism_reading, church_institutional_leadership).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__institutional_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint solves an institutional survival problem: how to maintain the church's organizational coherence, political standing, and property rights in the face of federal coercion that makes plural marriage untenable as a public practice. The coordination function is NOT the plural marriage doctrine itself—the coordination function emerges only AFTER the federal coercive pressure makes that doctrine unsustainable. The constraint coordinates the leadership's need to preserve the institution with the rank-and-file's need to maintain faith identity without legal jeopardy.
% TRANSFER_FUNCTION: Transfers institutional legitimacy from the legal/political domain (where federal law now dominates) to the doctrinal/prophetic domain (where the leadership claims revealed authority). The leadership claims the Manifesto as divine instruction, not capitulation. This reframing transfers the cost of abandonment from 'the institution lost a revealed doctrine' to 'God revealed a new phase.' It also transfers membership compliance from voluntary acceptance to identity-locked obedience: members must accept the reversal as legitimate prophetic teaching or face marginalization. The extraction moves upward: the institutional leadership gains restored rights and property; members lose doctrinal coherence and relational integrity (polygamists lose marriages; monogamists lose faith in institutional teaching).
% ABSENT_VOICES: Coerced polygamists and their families are structurally excluded from the leadership's deliberation. Rank-and-file members are announced the decision but not consulted. Federal pressure is the external constraint the leadership responds to, not a voice in the institutional conversation. Historians and documentary evidence (secret continuations, leadership diaries) later attests to the gap between the public doctrine (abandonment) and the private practice (continuation), but that evidence is not part of the 1890 institutional conversation itself.
% DISAPPEARANCE_RATIONALE: If the 1890 Manifesto and its enforcement mechanism vanished—if the institutional leadership had not authored the doctrinal suspension narrative—the federal coercion would still force legal abandonment of plural marriage, but the church would lack a legitimating narrative for the change. Members would understand it as capitulation, not divine instruction. The institution's restored political rights and property would depend on federal satisfaction that plural marriage was abandoned; without the doctrinal narrative to tie abandonment to revealed authority, the membership's obedience would be harder to secure and the institutional authority structure would be visibly compromised. The constraint's disappearance would require either acceptance of plural marriage as illegitimate (removing the doctrinal ground entirely) or acceptance of capitulation as capitulation (removing the legitimating narrative). Both reorient the institution's relationship to authority and membership.
% FOUNDING_PROBLEM: Federal law criminalized plural marriage and applied economic and political pressure to the church to compel abandonment of the practice. The church leadership faced institutional dissolution unless it could demonstrate public compliance with federal law while maintaining enough internal coherence to function as a religious organization.
% FOUNDING_PROBLEM_CORROBORATION: Federal coercion succeeded: Utah achieved statehood (1896), the church regained political standing, and plural marriage as a public institutional practice was discontinued. The 'founding problem' of institutional coercion is resolved BY the constraint's success. However, the founding problem that the constraint was DESIGNED to solve—how to abandon plural marriage while maintaining it was divinely authorized—is not solved by disappearance of the coercive pressure; it is solved by the reframing itself. Historians outside the benefiting institution (federal policymakers, academic historians, documentary evidence from the period) attest that the federal pressure was the driver and that the leadership's choice was constrained. Plural marriage advocates and sympathetic historians attest that the constraint represents capitulation dressed as revelation. The founding problem's status is contested by its very nature—the institutional pragmatism reading marks its resolution as strategic narrative-management, not spiritual revelation.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__institutional_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__institutional_pragmatism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__institutional_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply at 1890 (0.78) because the constraint's function shifts from institutionalizing an embraced doctrine to managing forced abandonment under a legitimating narrative. The baseline (1880, 0.15) reflects a period where plural marriage was an openly defended institutional position; the 1887 measurement (0.35) captures rising federal pressure and leadership awareness of institutional jeopardy. The 1890 peak (0.78) marks the Manifesto's crystallization as an enforced doctrinal reframing; extractiveness remains high through 1896 (0.76) despite federal pressure satisfaction because the constraint's enforcement continues—the institutional leadership must maintain the legitimating narrative even as external coercion relaxes. By 1910 (0.68), extractiveness declines as the constraint's function normalizes (the reframing becomes accepted narrative, not actively defended against internal dissent) and federal threat recedes, though the M-set gap persists (secret continuations documented through 1904 show the constraint never achieved full behavioral compliance). Theater ratio traces a similar arc: low (0.05) when plural marriage was an openly defended doctrine, spiking to 0.72 at the Manifesto itself (the reframing is the performative act) and remaining elevated (0.70-0.74) as the leadership maintains the narrative against internal and external skepticism. Suppression follows federal pressure: low (0.20) before intensified coercion, high (0.81) at 1890 when institutional enforcement machinery activates to suppress coerced polygamists and suppress documentation of secret continuations, declining (0.74) by 1910 as suppression institutionalizes into normalized exclusion. All measurements are authored on one shared time grid (six time points per metric) so temporal analysis has complete data. The cyclical elements are not oscillation but drift with plateaus: rise to crisis, sharp reframing, plateau of enforcement, gradual normalization as the reframed narrative becomes established.
 *
 * PERSPECTIVAL GAP:
 *   From the leadership's perspective: the Manifesto is pragmatic institutional survival in response to external coercion; it preserves the organization and, from that perspective, preserves the possibility of later doctrinal development or secret practice. From the coerced polygamists' perspective: the Manifesto is institutional betrayal—the doctrine they entered plural marriage under is reversed and they are expelled from the community or marginalized if they resist the reversal. From deceived monogamists' perspective: the Manifesto is epistemic reversal—the doctrine they accepted as revealed truth is now suspended, implicitly reframing years of teaching as provisional. From the federal government's perspective: the Manifesto is success—the institutional leadership has capitulated and demonstrated public compliance, achieving the political objective. The engine computes per-seat classifications from these structural positions; the leadership's seat should classify as a beneficiary (or possibly agenda-setter beneficiary for a rope), while the coerced and deceived seats should classify as payer/victim seats for a snare or tangled_rope. The claimed type (tangled_rope) asserts that both coordination (institutional survival) and asymmetric extraction (beneficiary leadership / victim members) are present in the same structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The leadership sits at d near 0.0 (full beneficiary): the constraint secures restored political rights, property, statehood, and maintains institutional authority—the leadership collects these gains directly and authoritatively. Coerced polygamists sit at d near 1.0 (full target): they bear the cost of family dissolution enforced through institutional pressure and identity-locked attachment. Deceived monogamists sit at d = 0.8-0.85 (near-full target): they bear epistemic costs and relational disruption; they believed in the doctrine as taught and experience its reversal as institutional betrayal. Rank-and-file members are distributed across d = 0.7-0.85 depending on whether they embraced the doctrinal teaching and how much they rely on the institutional identity. Federal government sits outside the institutional constraint structure—it applies external coercion, so d is not properly defined for it in the constraint's coordinate frame. Academic observers sit at d = 0.5 (symmetric, analytical). The core directionality asymmetry is between the leadership (d ~ 0.1, beneficiary, constrained) and the identity-locked members (d ~ 0.8, targets), mediated by federal external pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandatrophy status is contested (foundational to this reading). The founding mandate was plural marriage as divinely required. The 1890 Manifesto suspends the mandate but claims the suspension itself is divinely revealed. This creates a logical puzzle: if plural marriage was a mandate and the mandate is now suspended by divine revelation, was the mandate genuine (and thus its suspension requires explaining) or provisional (which reframes the entire prior teaching as less than fully binding)? The institutional pragmatism reading resolves this by treating the Manifesto's reframing narrative itself as the constraint: the constraint is the mechanism by which the leadership legitimates mandate abandonment by narrating it as new revelation. From this reading's perspective, mandatrophy is PARTIAL—the original mandate has functionally died (plural marriage is no longer institutionally supported), but the constraint (the reframing narrative) exists precisely to prevent the membership from recognizing the mandate as dead. Instead, the membership is directed to understand the mandate as having been temporally suspended by divine will. The constraint succeeds as long as this narrative is maintained; it fails if the membership comes to understand the mandate as abandoned due to federal coercion rather than divine instruction. The M-set gap (doctrine unchanged, practice suspended, secret continuations) is the empirical signature of this partial mandatrophy: the doctrine is never officially revoked (that would make mandatrophy explicit and visible), but the practice is officially abandoned and secret continuations are suppressed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_versus_narrative,
    'Is the constraint the plural marriage mandate itself, or is the constraint the Manifesto''s reframing narrative that legitimates the mandate''s suspension?',
    'Textual and historical analysis of leadership documents, private diaries, and institutional records: if the leadership treated the mandate as divinely binding and the suspension as tragic necessity imposed by external coercion, the constraint is the reframing narrative (pragmatism reading). If the leadership treated the mandate as always provisional or subject to revision, the constraint is a different phenomenon.',
    'If the narrative is the constraint, the constraint persists because the membership accepts the reframing; if the mandate was always understood as revisable by the leadership, the constraint is merely the exercise of normal institutional authority, not pragmatic adaptation to coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_versus_narrative, empirical, 'Whether the constraint is the mandate or its legitimating narrative.').

omega_variable(
    m_set_gap_causality,
    'Does the M-set gap (doctrine unchanged, practice suspended, secret continuations) arise because the leadership cynically authored the Manifesto as theater, or because the leadership genuinely struggled to reconcile doctrinal commitment with federal coercion?',
    'Leadership testimony (available through institutional archives, oral histories, minutes); psychological and institutional analysis of the leadership''s actual beliefs vs. public statements. A true gap between private conviction (mandate is binding) and public narrative (mandate is suspended) supports the pragmatism reading; a gap between leadership uncertainty (is the mandate truly binding?) supports a different reading.',
    'If the leadership knowingly authored the Manifesto as theater for institutional survival, the constraint is purely extractive (pragmatic adaptation). If the leadership genuinely believed the Manifesto as divine instruction but struggled with members who did not, the constraint is more mixed (coordination + authority assertion, less pure extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(m_set_gap_causality, empirical, 'Whether the Manifesto''s narrative reframing was consciously pragmatic or spiritually sincere.').

omega_variable(
    identity_lock_internalization,
    'Is the suppression measured as base_properties.suppression (institutional pressure on polygamists and skeptics) structural (membership can leave the institution) or internalized (members fused with the identity cannot psychologically exit even if institutional barriers are removed)?',
    'Post-1910 member trajectories: do members who leave the institution after accepting the Manifesto carry the suppression with them (evidence of internalization), or does the suppression dissipate once institutional barriers are removed (evidence of structural suppression)?',
    'If internalized, the measured suppression understates the constraint''s effective suppressive force; members leaving the institution may carry internal belief structures that continue to regulate their behavior around the authority they rejected. If structural, the suppression is primarily institutional enforcement machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'Whether the constraint''s suppression is structural institutional coercion or internalized identity fusion.').

omega_variable(
    reading_kernel_boundary,
    'Is this constraint (institutional_pragmatism_reading) a competing reading of the same Manifesto kernel, or is it a description of the Manifesto''s actual institutional function that applies across all readings?',
    'If endogenous_reinterpretation_reading and exogenous_override_reading, when authored, can both acknowledge that the Manifesto''s institutional function is pragmatic survival under coercion while still holding their respective readings, the pragmatism description is a fact about the constraint''s operation independent of reading (not a reading itself). If the other readings require denying the pragmatic function, then pragmatism is a competing reading.',
    'If pragmatism is a reading (competing with others), it enters the cs_structure.reading_relations set. If pragmatism is an operational fact independent of reading, it is background context, not a sibling reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_boundary, conceptual, 'Whether institutional pragmatism is a reading or a fact about the constraint''s operation common to all readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 1880, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1880, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1880, 0.05).
narrative_ontology:measurement_basis(plur_tr_t1880, observed).
narrative_ontology:measurement(plur_tr_t1887, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1887, 0.15).
narrative_ontology:measurement_basis(plur_tr_t1887, observed).
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1890, 0.72).
narrative_ontology:measurement_basis(plur_tr_t1890, observed).
narrative_ontology:measurement(plur_tr_t1896, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1896, 0.74).
narrative_ontology:measurement_basis(plur_tr_t1896, observed).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1904, 0.7).
narrative_ontology:measurement_basis(plur_tr_t1904, observed).
narrative_ontology:measurement(plur_tr_t1910, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1910, 0.65).
narrative_ontology:measurement_basis(plur_tr_t1910, observed).

% Extraction over time
narrative_ontology:measurement(plur_be_t1880, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement_basis(plur_be_t1880, observed).
narrative_ontology:measurement(plur_be_t1887, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1887, 0.35).
narrative_ontology:measurement_basis(plur_be_t1887, observed).
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1890, 0.78).
narrative_ontology:measurement_basis(plur_be_t1890, observed).
narrative_ontology:measurement(plur_be_t1896, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1896, 0.76).
narrative_ontology:measurement_basis(plur_be_t1896, observed).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1904, 0.72).
narrative_ontology:measurement_basis(plur_be_t1904, observed).
narrative_ontology:measurement(plur_be_t1910, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1910, 0.68).
narrative_ontology:measurement_basis(plur_be_t1910, observed).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1880, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1880, 0.2).
narrative_ontology:measurement_basis(plur_su_t1880, observed).
narrative_ontology:measurement(plur_su_t1887, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1887, 0.58).
narrative_ontology:measurement_basis(plur_su_t1887, observed).
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1890, 0.81).
narrative_ontology:measurement_basis(plur_su_t1890, observed).
narrative_ontology:measurement(plur_su_t1896, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1896, 0.8).
narrative_ontology:measurement_basis(plur_su_t1896, observed).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1904, 0.78).
narrative_ontology:measurement_basis(plur_su_t1904, observed).
narrative_ontology:measurement(plur_su_t1910, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1910, 0.74).
narrative_ontology:measurement_basis(plur_su_t1910, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__institutional_pragmatism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(plural_marriage_mandate__institutional_pragmatism_reading, 0.12).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The plural_marriage_mandate kernel decomposes into three structurally distinct constraints (one per reading). The institutional_pragmatism_reading treats the constraint as the reframing narrative mechanism (ε = 0.78, extraction through legitimation). The endogenous_reinterpretation_reading treats the constraint as the Manifesto as genuine doctrinal development (ε lower, mountain-candidate, no extraction from reframing). The exogenous_override_reading treats the constraint as federal coercion succeeding where institutional doctrine never held (ε high for federal enforcement, but the Manifesto is a symptom, not the constraint). All three share the same referent (the 1890 Manifesto) but read different structural roles into it. The pragmatism reading influences both siblings by establishing the M-set gap and federal coercion as empirical facts that any reading must accommodate; sibling readings coexist with pragmatism in different parties' accounts of what the Manifesto represents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(plural_marriage_mandate__institutional_pragmatism_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
