% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__substitutionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__substitutionist_reading, []).

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
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Divine Marriage Command (Substitutionist Reading): Monogamy as Doctrinal Mandate
 *   domain: religious/political_theology/commitment_systems
 *
 * SUMMARY:
 *   The substitutionist reading declares that the 1890 Manifesto represents a
 *   new, binding revelation from the divine authority, superseding prior
 *   doctrine permitting plural marriage. Under this reading, monogamy becomes
 *   obligatory and polygamy becomes apostasy. The constraint operates at the
 *   intersection of religious authority (the claim to revelation), political
 *   necessity (federal legal pressure), and institutional membership
 *   (belonging requires doctrinal alignment). The reading's core structural
 *   claim is that the Manifesto is a legitimate exercise of the authority to
 *   redefine doctrine through fresh revelation, not a capitulation to federal
 *   coercion. This reading competes with continuationist and
 *   coercion-visibility siblings; the three readings share a kernel (the
 *   marriage teaching) but instantiate different constraints with different
 *   victim sets and justification structures.
 *
 * KEY AGENTS:
 *   - institutional_leadership: Agenda-setter; controls the authority to declare doctrine and enforce orthodoxy; benefits from consolidated authority and restored institutional legitimacy.
 *   - polygamist_practitioners: Targets; treated as apostates post-Manifesto; identity-locked and constrained by kinship and community dependence.
 *   - fundamental_dissenters: Powerless targets; reject the reading's authority claim; trapped by institutional dependence with no voice in the authority structure.
 *   - monogamist_faithful: Organized beneficiaries; vindicated by doctrinal clarification; strengthened institutional standing.
 *   - federal_government: Structurally present but formally excluded; the historical pressure that the reading must not acknowledge as causal.
 *   - theological_scholars: Analytical observers; examine the reading's internal coherence and distinguish it from sibling readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.68).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.71).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Divine Marriage Command (Substitutionist Reading): Monogamy as Doctrinal Mandate").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, '59bd93fd-7387-4a67-afda-755e623ec8f1').
narrative_ontology:cs_kernel_codification('59bd93fd-7387-4a67-afda-755e623ec8f1', fixed_text).
narrative_ontology:cs_authority_grounding('59bd93fd-7387-4a67-afda-755e623ec8f1', lineage).
narrative_ontology:cs_interpretation_layer_present('59bd93fd-7387-4a67-afda-755e623ec8f1').
narrative_ontology:cs_reading_relation('59bd93fd-7387-4a67-afda-755e623ec8f1', divine_marriage_command__continuationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('59bd93fd-7387-4a67-afda-755e623ec8f1', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_axiom('59bd93fd-7387-4a67-afda-755e623ec8f1', foundational, manifesto_is_revealed_doctrine).
narrative_ontology:cs_axiom_status(manifesto_is_revealed_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('59bd93fd-7387-4a67-afda-755e623ec8f1', manifesto_is_revealed_doctrine, deontological).
narrative_ontology:cs_axiom('59bd93fd-7387-4a67-afda-755e623ec8f1', foundational, institutional_authority_to_rescind_prior_revelation).
narrative_ontology:cs_axiom_status(institutional_authority_to_rescind_prior_revelation, holdable).
narrative_ontology:cs_axiom_grounding('59bd93fd-7387-4a67-afda-755e623ec8f1', institutional_authority_to_rescind_prior_revelation, conventional).
narrative_ontology:cs_reference_frame('59bd93fd-7387-4a67-afda-755e623ec8f1', divinely_revealed_polygamy_doctrine).
narrative_ontology:cs_drift_state('59bd93fd-7387-4a67-afda-755e623ec8f1', post_manifesto_monogamy_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('59bd93fd-7387-4a67-afda-755e623ec8f1', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, monogamist_faithful).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, polygamist_practitioners).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, fundamental_dissenters).
narrative_ontology:constraint_vindicates(divine_marriage_command__substitutionist_reading, doctrinal_supremacy_of_revelation).
narrative_ontology:constraint_vindicates(divine_marriage_command__substitutionist_reading, ecclesiastical_authority_to_rescind_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The church's central authority that promulgated the Manifesto as new revealed doctrine. Sets the boundary between orthodoxy and heresy. Frames the Manifesto as divine instruction rather than response to federal pressure, grounding institutional legitimacy in doctrinal continuity and revelation. Enforces compliance through exclusion, excommunication, and control over priesthood access.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, continental).

% Members who practiced or sought to practice polygamy in alignment with prior doctrine. After the Manifesto, they face excommunication, family fragmentation, and spiritual exile if they continue. Their identity as practitioners of revealed truth now defines them as apostates. Leaving the institution means losing kinship networks, community belonging, and the spiritual framework that organized their worldview.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, polygamist_practitioners, payer,
    moderate, generational, identity_locked, regional).

% Those who reject the Manifesto's authority as revelation—arguing it is capitulation to federal coercion masked in theological language. They have no institutional platform to voice this view; institutional communication channels are controlled by leadership. Forced choice: accept the new doctrine (spiritual disenfranchisement), leave and lose community entirely (structural entrapment through kinship and economic dependence), or resist clandestinely (risk excommunication and public denunciation).
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, fundamental_dissenters, payer,
    powerless, biographical, trapped, local).

% Members whose practice already aligned with the new requirement; members newly convinced of monogamy's righteousness. They benefit from institutional clarification, enhanced social respectability in the broader society, and consolidation of leadership authority around a unified doctrine. Their compliance is rewarded with full participation and institutional standing.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, monogamist_faithful, beneficiary,
    organized, generational, mobile, continental).

% The historical pressure agent (anti-polygamy statute and enforcement). Formally excluded from the theological framing, yet structurally present: the Manifesto's timing and content track federal legal pressure. The institutional narrative requires that external coercion be reframed as internal revelation to maintain the authority structure's legitimacy claim.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, federal_government, excluded,
    institutional, biographical, analytical, national).

% Academic observers analyzing the reading's internal consistency: whether revelation can authentically reverse prior revelation; whether the reading's theological coherence depends on denying the causal role of federal pressure; what markers would distinguish a genuine new revelation from a necessitated doctrinal pivot.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, theological_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__substitutionist_reading, institutional_leadership).
narrative_ontology:fixing_cost_class(divine_marriage_command__substitutionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes unambiguous institutional doctrine on marriage form: all members align on a single authorized teaching, removing internal debate and creating unified external representation to civil authority. Solves the coordination problem of unified theology and institutional boundary-drawing around orthodoxy vs. heresy.
% TRANSFER_FUNCTION: Moves authority and legitimacy from polygamist practitioners (whose prior alignment with doctrine is now redefined as apostasy) to institutional leadership (authority to define and redefine doctrine is confirmed and concentrated) and monogamist faithful (now vindicated as correct practitioners). Also moves social respectability from the institution to mainstream society, and moves compliance from voluntary doctrinal preference to coerced orthodoxy.
% ABSENT_VOICES: Federal authorities and their legal pressure are structurally absent from the theological narrative. Fundamentalist dissenters and continuationists are excluded from authority channels; they would testify that the Manifesto is institutional capitulation to state coercion disguised as revelation, but institutional communication is controlled. Historical polygamist practitioners whose lives are reorganized by the doctrine have no platform to name their treatment as excommunication rather than faithful adjustment.
% DISAPPEARANCE_RATIONALE: If this constraint (the Manifesto's doctrine enforced as binding revelation) disappeared, the institution would reorganize around continuationist or pluralist readings; practitioners would reconnect across the schism; institutional relationship to federal law would require explicit renegotiation rather than being embedded in doctrine; the authority structure's claims about revelation would face direct contestation again.
% FOUNDING_PROBLEM: Prior doctrine permitted plural marriage; federal law prohibited it; institutional survival under federal jurisdiction required doctrinal adjustment. The founding problem is the collision between theological precedent and political authority.
% FOUNDING_PROBLEM_CORROBORATION: Historians and archivists (outside the benefiting parties) document the timeline: Edmunds Act 1882, escalating federal prosecution, institutional property loss, institutional leadership statements about survival necessity. The founding problem—the collision between prior doctrine and federal law—is resolved by the constraint itself. However, the MANIFESTATION of the founding problem (whether the constraint represents theological truth or political necessity) is actively contested by continuationist and coercion-visibility readings, whose corroborating sources include historical documents from the institutional record that institutional leadership has recontextualized.
narrative_ontology:disappearance_verdict(divine_marriage_command__substitutionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__substitutionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__substitutionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__substitutionist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__substitutionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__substitutionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint systematically reorganizes who counts as orthodox and who is excluded; institutional leadership concentrates authority and legitimacy; polygamist practitioners lose standing and community access. Suppression is high (0.71) because the reading's persistence depends on preventing fundamental dissenters from gaining platform and on constraining polygamist practitioners through identity-lock (kinship, community belonging, spiritual framework). Theater is at the threshold (0.52) because a substantial portion of institutional activity is devoted to maintaining the narrative that the Manifesto is new revelation rather than political adaptation—the 'security review' function of teaching and enforcement machinery is real but intertwined with legitimacy-theater. Accessibility collapse is high (0.79): once the reading frames monogamy as doctrine, alternatives (continuationist, pluralist readings) are pushed to schism and excommunication; reframing requires exit from the institution. Resistance is moderate (0.58): organized resistance emerges from discontinuist/continuationist movements and from federal challenge to institutional authority itself, but the institutional communication apparatus constrains public contestation. The measurement series shows extraction rising sharply in the first 15 time-points (years 0–15 post-Manifesto, as enforcement hardens and dissenters are excommunicated), then plateauing as the doctrine becomes internalized and theater-ratio stabilizes around the maintenance level. This is consistent with a constraint whose initial enforcement phase is extractive and whose steady-state operation balances genuine coordination (unified doctrine for monogamist faithful) with ongoing suppression (exclusion of dissenters).
 *
 * PERSPECTIVAL GAP:
 *   The perspective divergence between institutional leadership and polygamist practitioners should be dramatic: from leadership's seat, the constraint solves the coordination problem of unified doctrine and institutional survival under federal pressure—a genuine tangled_rope with real benefits (monogamist faithful get clarity, institution survives). From the polygamist-practitioner seat, the constraint is a snare: they are reclassified as apostates retroactively, face excommunication and family fragmentation, and have no voice in the 'revelation' that dismantled their prior doctrinal standing. From the fundamental-dissenter seat, the constraint is even more extractive—pure snare with identity-lock and no beneficiary case at all. The engine computes all three per-seat types from the structural data; the claim/metric independence rule means the authored claim (tangled_rope, the reading's self-framing) diverges from what each seat experiences. This divergence IS the measurement the constraint story exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership's directionality is near 0.0 (full beneficiary): the constraint concentrates authority, collects legitimacy and compliance, faces minimal constraint from the doctrine. Polygamist practitioners sit near 0.95 (near-full target): they bear the entire cost of reclassification, face excommunication, are trapped by identity-lock, and receive nothing but exclusion. Fundamental dissenters sit at 0.98 (full target): powerless, trapped, paying through exclusion and loss of voice. Monogamist faithful sit near 0.2 (substantial beneficiary): they benefit from doctrinal vindication and unified teaching, pay no cost (their practice is now aligned), and face no suppression. Federal government sits at 0.5 (symmetric): benefited from the constraint's functional result (institutional monogamy, state-law alignment) but is formally excluded from the theological framing and cannot claim credit without undermining the reading's legitimacy claim. Theological scholars sit at 0.5 (analytical): neither benefiting nor paying, observing the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (collision between prior doctrine and federal law) is DEAD: federal enforcement of anti-polygamy statutes is now settled law and federal-institutional alignment is achieved. Yet the constraint persists because: (1) institutional leadership retains authority to enforce the doctrine; (2) polygamist practitioners remain identity-locked even if federal pressure ceased; (3) dissenters remain trapped by kinship/economic dependence. The persistence is not driven by solving the founding problem (which is solved); it is driven by consolidation of authority, suppression of dissent, and internalized identity-lock. This is a mandatrophy case: the constraint's justification (federal coercion requiring doctrinal adaptation) has expired, but the constraint persists as institutional inertia + enforced orthodoxy. The reading's vulnerability is that it CANNOT explicitly acknowledge the founding problem's death without undermining its own authority claim (if the problem is solved, why maintain the enforcement?). The theater_ratio's rise from 0.38 to 0.52 tracks this: as external pressure decreases, institutional activity shifts from response-to-crisis toward maintenance-of-orthodoxy-as-performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_coercion_framing,
    'Is the Manifesto a genuine new revelation from the divine authority, or is it institutional response to federal coercion that has been retrospectively framed as revelation?',
    'Examine contemporaneous institutional correspondence, leadership statements, and temporal alignment between federal legal pressure and doctrinal announcement. Compare institutional framing in internal vs. external communication. Assess whether the reading''s core claim—that revelation, not coercion, drives the doctrine—is distinguishable from the continuationist and coercion-visibility readings on evidential grounds, or only on framework premises.',
    'If coercion is the primary driver with revelation as cover narrative, the constraint''s legitimacy derivation collapses and effective extraction rises (the suppression mechanism becomes primary rather than secondary). The reading would reclassify from tangled_rope (coordination + extraction) toward snare (suppression masquerading as coordination). If revelation is genuine and independent of coercion, the coordination function is primary and the reading holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_vs_coercion_framing, conceptual, 'The relationship between divine revelation and institutional political necessity in the Manifesto''s authority claim.').

omega_variable(
    prior_doctrine_rescission_legitimacy,
    'Within the theological framework that grounded prior polygamy doctrine as divinely revealed, what mechanism legitimates rescinding that doctrine through new revelation? Does the reading''s authority structure permit arbitrary or frequent doctrinal reversal?',
    'Examine the reading''s account of doctrinal continuity and change: what criteria distinguish authentic new revelation from institutional opportunism? Compare the Manifesto''s justification against the tradition''s prior standards for doctrinal authority. Test whether the reading consistently applies its own criteria in other cases of doctrinal change.',
    'If the mechanism is coherent and consistently applied, the reading''s internal logic is stable and tangled_rope classification holds. If the mechanism is ad-hoc (revelation is invoked only when convenient), the theater_ratio rises above 0.52, indicating performative authority covering extraction; the constraint approaches piton character. If the prior doctrine is treated as not actually revealed (minimized retrospectively), the victim set expands and effective extraction on polygamist practitioners rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prior_doctrine_rescission_legitimacy, conceptual, 'The theological legitimacy of rescinding prior revelation through new revelation.').

omega_variable(
    identity_lock_persistence,
    'Is the suppression of polygamist practitioners and fundamental dissenters primarily structural (economic, kinship, institutional) or internalized (the practitioners have adopted the reading''s framework and now experience their prior practice as apostate)?',
    'Post-schism trajectory: track whether dissenters who exit the institution retain their prior theological framework and community, or whether re-entrance requires interior adoption of the monogamist reading. Measure whether successor-generation born into the post-Manifesto institution hold the monogamist doctrine as freely chosen or as identity-constituted. Assess whether dissenter exit is reversible without requiring wholesale identity reconstruction.',
    'If primarily structural, the effective suppression is localized to those with high institutional dependence; exit is possible with reorganization cost. If internalized, the suppression persists after exit (dissenters carry identity-shame even outside the institution), making effective extraction on the identity-locked set substantially higher than measured suppression. The reading''s effective extraction then exceeds the authored base by 0.15–0.25 in the identity-locked seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether the suppression mechanism is structural or internalized in identity-locked practitioners.').

omega_variable(
    kernel_contest_framing_ambiguity,
    'This constraint is one reading of the divine_marriage_command kernel. The sibling readings (continuationist_reading, coercion_visibility_reading) instantiate materially different constraints with different victim sets and beneficiary structures. Is the choice between readings routed through theological evidence, institutional power, or narrative authority?',
    'Examine what would count as evidence to a practitioner for choosing between the readings. If evidence is theological (scriptural interpretation, hermeneutic consistency), the choice is contestable in principle. If evidence is institutional (who controls the authority structure to declare interpretation), the choice is a power structure, not a truth discovery. If evidence is narrative (what framing story the institution authoritatively tells), the choice is legitimacy-construction.',
    'This omega documents the irreducible uncertainty in the committer frame: the constraint''s classification as tangled_rope depends on the reading''s authority legitimation. If that legitimation itself is contested across the kernel''s readings, no single classification holds uniformly across all parties. The engine computes per-seat types; this omega records that the analytical seat must account for the kernel contest''s role in differentiating seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_framing_ambiguity, conceptual, 'The role of kernel contest in determining this reading''s classification across seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__substitutionist_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(divi_tr_t0, observed).
narrative_ontology:measurement(divi_tr_t5, divine_marriage_command__substitutionist_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement_basis(divi_tr_t5, observed).
narrative_ontology:measurement(divi_tr_t10, divine_marriage_command__substitutionist_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement_basis(divi_tr_t10, observed).
narrative_ontology:measurement(divi_tr_t15, divine_marriage_command__substitutionist_reading, theater_ratio, 15, 0.49).
narrative_ontology:measurement_basis(divi_tr_t15, observed).
narrative_ontology:measurement(divi_tr_t20, divine_marriage_command__substitutionist_reading, theater_ratio, 20, 0.51).
narrative_ontology:measurement_basis(divi_tr_t20, observed).
narrative_ontology:measurement(divi_tr_t25, divine_marriage_command__substitutionist_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement_basis(divi_tr_t25, observed).
narrative_ontology:measurement(divi_tr_t30, divine_marriage_command__substitutionist_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement_basis(divi_tr_t30, observed).
narrative_ontology:measurement(divi_tr_t40, divine_marriage_command__substitutionist_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement_basis(divi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__substitutionist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(divi_be_t0, observed).
narrative_ontology:measurement(divi_be_t5, divine_marriage_command__substitutionist_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(divi_be_t5, observed).
narrative_ontology:measurement(divi_be_t10, divine_marriage_command__substitutionist_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(divi_be_t10, observed).
narrative_ontology:measurement(divi_be_t15, divine_marriage_command__substitutionist_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(divi_be_t15, observed).
narrative_ontology:measurement(divi_be_t20, divine_marriage_command__substitutionist_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(divi_be_t20, observed).
narrative_ontology:measurement(divi_be_t25, divine_marriage_command__substitutionist_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(divi_be_t25, observed).
narrative_ontology:measurement(divi_be_t30, divine_marriage_command__substitutionist_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(divi_be_t30, observed).
narrative_ontology:measurement(divi_be_t40, divine_marriage_command__substitutionist_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(divi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__substitutionist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(divi_su_t0, observed).
narrative_ontology:measurement(divi_su_t5, divine_marriage_command__substitutionist_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(divi_su_t5, observed).
narrative_ontology:measurement(divi_su_t10, divine_marriage_command__substitutionist_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(divi_su_t10, observed).
narrative_ontology:measurement(divi_su_t15, divine_marriage_command__substitutionist_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(divi_su_t15, observed).
narrative_ontology:measurement(divi_su_t20, divine_marriage_command__substitutionist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(divi_su_t20, observed).
narrative_ontology:measurement(divi_su_t25, divine_marriage_command__substitutionist_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(divi_su_t25, observed).
narrative_ontology:measurement(divi_su_t30, divine_marriage_command__substitutionist_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(divi_su_t30, observed).
narrative_ontology:measurement(divi_su_t40, divine_marriage_command__substitutionist_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(divi_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__substitutionist_reading, 0.12).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% The divine_marriage_command kernel is instantiated by three readings, each constituting a structurally distinct constraint: substitutionist_reading (monogamy as new doctrine via revelation), continuationist_reading (polygamy remains doctrinally valid; Manifesto is prudential suspension), coercion_visibility_reading (Manifesto is institutional response to federal coercion; legitimacy derives from survival necessity). The epsilon values differ substantially—substitutionist_reading centers extraction around identity-lock and doctrinal authority; continuationist_reading centers it around institutional coercion and schism; coercion_visibility_reading centers it around transparency of the legitimacy claim. Each reading has a different victim/beneficiary structure. The three stories are linked via network.affects_constraints because they compete for institutional authority and practitioner allegiance; each reading materially affects the others' viability and the institutional landscape each describes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__substitutionist_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
