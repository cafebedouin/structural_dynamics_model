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
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Divine Marriage Command (Substitutionist Reading): Monogamy Doctrine Post-Manifesto
 *   domain: religious/political_theology/commitment_systems
 *
 * SUMMARY:
 *   The religious institution historically taught that polygamous marriage
 *   was doctrinally permitted and (in some periods) required. Federal legal
 *   pressure—anti-polygamy laws, land seizures, statehood denial—created an
 *   existential institutional crisis. The Manifesto declared that monogamy
 *   alone is now doctrinally required, framed as a new revelation from the
 *   same divine source that authorized polygamy. Under the substitutionist
 *   reading instantiated here, the Manifesto is a genuine new revelation, not
 *   a capitulation; monogamy becomes binding doctrine; practitioners of
 *   historical polygamy become apostate; and institutional leadership gains
 *   the authority to declare and enforce doctrinal revisions. This creates a
 *   tangled_rope constraint: a coordination function (unified doctrine
 *   enabling membership coherence) is yoked to an extraction mechanism
 *   (redistribution of doctrinal authority from prior doctrine to
 *   institutional leadership, with costs borne by practitioners of historical
 *   family forms). The claim/metric gap is deliberate and structural: the
 *   constraint is CLAIMED as tangled_rope (coordination + enforcement), and
 *   the metrics describe extraction (0.68), suppression (0.72), and moderate
 *   theater (0.58) consistent with that claim. The measurement series traces
 *   extraction rising from 0.31 at the crisis moment (federal pressure) to
 *   0.68 at institutional stabilization, while theater declines from 0.72
 *   (high apologetics and reaffirmation) to 0.58 (settlement into normalized
 *   practice). The suppression trajectory rises sharply early (enforcement of
 *   doctrinal change, excommunication of resisters) and plateaus (sustained
 *   suppression of continuationist dissent). All measurements share one time
 *   grid.
 *
 * KEY AGENTS:
 *   - institutional_leadership: Declares and enforces the monogamy doctrine; benefits from unified authority structure; power=institutional, exit=constrained.
 *   - practitioners_of_historical_polygamy: Victims of the doctrinal shift; identity_locked due to religious/community fusion; power=powerless.
 *   - doctrinal_conservatives: Contesters of the Manifesto's legitimacy; claim prior doctrine is binding; power=moderate, exit=constrained.
 *   - general_membership: Beneficiaries of institutional consolidation and social legitimacy; power=organized.
 *   - federal_authorities: Exogenous enforcer of the crisis condition; excluded from the theological framing that sustains the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.68).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.72).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Divine Marriage Command (Substitutionist Reading): Monogamy Doctrine Post-Manifesto").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, '33a10dce-4d02-484d-82b7-2bd9177e73fb').
narrative_ontology:cs_kernel_codification('33a10dce-4d02-484d-82b7-2bd9177e73fb', fixed_text).
narrative_ontology:cs_authority_grounding('33a10dce-4d02-484d-82b7-2bd9177e73fb', lineage).
narrative_ontology:cs_interpretation_layer_present('33a10dce-4d02-484d-82b7-2bd9177e73fb').
narrative_ontology:cs_reading_relation('33a10dce-4d02-484d-82b7-2bd9177e73fb', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_reading_relation('33a10dce-4d02-484d-82b7-2bd9177e73fb', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('33a10dce-4d02-484d-82b7-2bd9177e73fb', foundational, manifesto_constitutes_binding_revelation).
narrative_ontology:cs_axiom_status(manifesto_constitutes_binding_revelation, holdable).
narrative_ontology:cs_axiom_grounding('33a10dce-4d02-484d-82b7-2bd9177e73fb', manifesto_constitutes_binding_revelation, deontological).
narrative_ontology:cs_axiom('33a10dce-4d02-484d-82b7-2bd9177e73fb', foundational, institutional_leadership_authorized_to_supersede_prior_doctrine).
narrative_ontology:cs_axiom_status(institutional_leadership_authorized_to_supersede_prior_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('33a10dce-4d02-484d-82b7-2bd9177e73fb', institutional_leadership_authorized_to_supersede_prior_doctrine, deontological).
narrative_ontology:cs_reference_frame('33a10dce-4d02-484d-82b7-2bd9177e73fb', prior_polygamy_doctrine_binding).
narrative_ontology:cs_drift_state('33a10dce-4d02-484d-82b7-2bd9177e73fb', post_manifesto_consolidation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('33a10dce-4d02-484d-82b7-2bd9177e73fb', '2026-06-12T14:23:45Z').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, monogamous_doctrine_interpreters).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, practitioners_of_historical_polygamy).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, doctrinal_conservatives).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, general_membership).
narrative_ontology:constraint_vindicates(divine_marriage_command__substitutionist_reading, modern_moral_progress_doctrine).
narrative_ontology:constraint_vindicates(divine_marriage_command__substitutionist_reading, institutional_doctrinal_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the interpretation and promulgation of doctrine. Declares the Manifesto a new revelation that supersedes the prior polygamy command. Enforces monogamy requirement through disciplinary mechanisms and frames the shift as theological development rather than institutional accommodation to external pressure. Legitimate authority over membership depends on the authority to declare binding doctrinal revisions.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Scholars, theologians, and leaders who endorse monogamy as the binding doctrine post-Manifesto. Their credibility and institutional position depend on the Manifesto's legitimacy as a genuine revelation. They benefit from coherence: if monogamy is doctrinally required, their entire interpretive framework is vindicated and their institutional authority is secured.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, monogamous_doctrine_interpreters, beneficiary,
    organized, generational, mobile, global).

% Individuals and families whose religious practice and family arrangements were doctrinally valid under prior doctrine but become apostasy under the new requirement. They face a choice: dissolve plural marriages (with severe social and economic consequences), exit the institution (forfeiting community, belief framework, and identity), or maintain practice and accept excommunication. Identity fusion—their sense of self, family structure, and religious belonging are constitutively tied to the institution—makes exit theoretically available but psychologically prohibitive.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, practitioners_of_historical_polygamy, payer,
    powerless, biographical, identity_locked, local).

% Faithful believers who maintain that the prior polygamy doctrine was a binding revelation and that institutional leadership cannot supersede it via a new revelation. They read the Manifesto as capitulation to federal pressure disguised as theological innovation. Their position is doctrinal: they claim the authority structure is bound by prior doctrine and lacks the standing to declare a rescission. Excommunication becomes the enforcement mechanism against their refusal to accept the new doctrine.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, doctrinal_conservatives, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__substitutionist_reading, doctrinal_conservatives, excluded).

% Applied legal and political pressure against polygamy; statehood admission and land seizure are contingent on doctrinal change. They are structurally excluded from the institutional leadership's theological framing—the Manifesto's legitimacy claim depends on naming it as revelation, not as response to coercion. Acknowledging federal pressure as the driver would undermine the Manifesto's standing as binding doctrine within the theological tradition.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, federal_authorities, excluded,
    institutional, biographical, trapped, national).

% Adherents who accept the new monogamy doctrine. They benefit from the institutional consolidation and social legitimacy the Manifesto provides; the shift aligns the institution with surrounding secular norms, reducing persecution and increasing access to wider society. Their coordination around a single moral framework (monogamy as divine requirement) is enabled by the Manifesto's authority claim.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, general_membership, beneficiary,
    organized, biographical, constrained, global).

% Scholars and analysts external to the institution studying the constraint: the process by which a prior binding doctrine (polygamy permitted/required) becomes superseded by a new one (monogamy required) and the mechanisms by which that transition is legitimated within the tradition's own framework.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, theological_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__substitutionist_reading, institutional_leadership).
narrative_ontology:fixing_cost_class(divine_marriage_command__substitutionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates institutional membership around a unified moral doctrine regarding marriage: before the Manifesto, plural marriage was doctrinally permissible; after, monogamy alone is doctrinally required. This coordination function solves the problem of doctrinal coherence within the institutional framework—members need a single, authoritative statement of binding obligations.
% TRANSFER_FUNCTION: Transfers doctrinal authority from the prior revelation (which permitted/required polygamy) to the new revelation (which requires monogamy). This moves legitimacy from doctrinal conservatives (who defend prior doctrine) to institutional leadership (who declare and interpret the new doctrine). It also transfers social positioning: families practicing historical polygamy lose institutional standing; adherents practicing monogamy gain it. External political pressure (federal anti-polygamy law) is transformed into internal theological doctrine, moving the constraint's framing from external coercion to divine command.
% ABSENT_VOICES: Practitioners of historical polygamy are partially present but politically voiceless—they have no seat in doctrinal authority and their testimony that the prior doctrine was binding is treated as apostasy, not as valid theological argument. Federal authorities are wholly absent from the theological framing, though their pressure is structurally decisive; acknowledging their role would undermine the Manifesto's legitimacy claim. Comparative theological voices (other traditions' marriage doctrines) are absent from the internal institutional conversation.
% DISAPPEARANCE_RATIONALE: If the monogamy requirement and the Manifesto's authority vanished overnight, the institution would reorganize around the prior doctrine (polygamy permitted) or splinter into continuationist and substitutionist factions (as historically occurred, producing FLDS and mainstream LDS schism). The constraint's disappearance would restore doctrinal validity to plural marriage and dissolve the grounds for excommunication of practitioners who maintained historical practice. Institutional membership and family structure would realign—families currently deemed apostate would be restored to good standing; families formed under monogamy doctrine would remain valid under prior doctrine as well.
% FOUNDING_PROBLEM: The institution faced federal legal penalties, land seizure, and statehood denial unless it abandoned the practice and doctrine of plural marriage. The Manifesto was issued as a response to this existential institutional threat. The founding problem is: how to reconcile doctrinal authority (the prior doctrine permitting/requiring polygamy was divinely given) with political survival (continued practice ensures institutional destruction).
% FOUNDING_PROBLEM_CORROBORATION: Institutional leadership frames the founding problem as purely theological: the problem is a divine revelation updating prior doctrine, and the new doctrine is binding because it comes from the same authoritative source. Historians and comparative religion scholars attest that the founding problem was political/legal pressure from federal authorities, with the theological framing retroactively applied to sustain institutional legitimacy. Congressional testimony on the anti-polygamy campaign and federal land seizures corroborate the political pressure; church historical archives (when accessible) document the calculation of institutional survival versus doctrinal integrity. Continuationist voices within and after the tradition attest the founding problem is not solved but suppressed—prior doctrine remains binding and the Manifesto is a prudential suspension under duress, not a rescission.
narrative_ontology:disappearance_verdict(divine_marriage_command__substitutionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__substitutionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness at 0.68 reflects the magnitude of doctrinal shift: the constraint redistributes authority (prior doctrine is superseded), redistributes legitimacy (prior practice becomes apostasy), and imposes costs on practitioners of historical forms (dissolution of plural families, excommunication, loss of community standing). The shift persists not because participants prefer it but because institutional leadership enforces it and framing it as revelation sustains that enforcement. Suppression at 0.72 reflects the active enforcement machinery: excommunication, doctrinal policing, teaching that dissent is apostasy, social pressure against continuationists. Theater at 0.58 (moderate) reflects a hybrid: some component is genuine (institutional leadership does undertake doctrinal interpretation as part of its authority structure) and some is cover (the Manifesto's framing as revelation partly obscures the federal pressure that drove it). The measurement trajectory (extraction rising as the doctrine consolidates, theater declining as acceptance settles, suppression rising early then holding) suggests the constraint transitions from crisis-response (high theater: repeated reaffirmations that this is revelation, not capitulation) to normalized doctrine (lower theater: monogamy is simply taught as binding, without constant apologetics). The accessibility_collapse at 0.61 reflects that alternatives (continuationist doctrine, acknowledgment that federal coercion drove the change) exist but are closed off by institutional authority—not naturally inaccessible but actively suppressed. The resistance at 0.54 reflects the continuationist opposition that persists (FLDS schism historically, ongoing doctrinal dissent) and the internal weight of prior doctrine (which practitioners learned as binding). Directionality: the institutional leadership sits near 0.0 (full beneficiary: collects authority, legitimacy, social acceptance); practitioners of historical polygamy sit near 1.0 (full target: lose family structure, community standing, religious status); doctrinal conservatives sit near 0.8 (mostly target: their position is suppressed and treated as apostasy); general membership sits near 0.2 (mostly beneficiary: gain institutional coherence and social legitimacy, though they also indirectly bear the cost of excluding or disciplining practitioners of historical forms).
 *
 * PERSPECTIVAL GAP:
 *   The Manifesto's legitimacy claim depends on framing it as revelation, not as institutional response to coercion. This framing allows institutional leadership to claim authority over doctrine without admitting that federal pressure is the actual driver. Practitioners of historical polygamy, and continuationists who defend prior doctrine, experience this as a false legitimacy claim—they see the constraint as coercive imposition justified by a false revelation narrative. The suppression mechanism (excommunication, doctrinal policing, teaching that dissent is apostasy) is partly structural (institutional mechanisms exist to enforce doctrine) and partly internalized (practitioners' religious identity is fused with the institution, making exit nominally possible but psychologically prohibitive). Distinguishing these components is the suppression_mechanism_identity_fusion omega.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional_leadership is the full beneficiary: it collects doctrinal authority (the power to declare binding doctrine), institutional legitimacy (the shift enables statehood and social acceptance), and continuity (the constraint prevents schism or institutional collapse). Its d is near 0.0. The practitioners_of_historical_polygamy are the full target: they lose family structures, religious status, and community membership. They are identity_locked (their sense of self and belonging is constituted through the institution and its prior doctrine). Their exit options are nominally mobile (they can leave) but identity-locked suppresses mobility—exit means psychological dissolution of the self-concept. Their d is near 1.0. Doctrinal_conservatives are mostly targets (their doctrine is suppressed, their dissent is treated as apostasy) but hold some moderate power (they can argue theology, maintain alternative communities). Their d is near 0.8. General_membership are mostly beneficiaries (they gain institutional coherence and social acceptance) but indirectly bear the cost of the constraint's suppression machinery. Their d is near 0.2. Directionality overrides: none required; the structural data (beneficiary/victim/power/exit) produces accurate d values without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem is: how to reconcile doctrinal authority (prior doctrine is binding) with institutional survival (continued practice ensures destruction). The founding_problem_status in the substitutionist reading is CONTESTED (institutional leadership claims the problem is solved by revelation; continuationists claim the problem is not solved but suppressed). The disappearance_verdict is WORLD_REARRANGES (if the monogamy constraint vanished, the institution would reorganize around prior doctrine or schism into factions). This mismatch (problem status=contested + disappearance=world_rearranges) is a zombie-constraint flag: the founding problem is not genuinely resolved; the constraint persists because institutional leadership enforces it, not because the problem is solved. The Manifesto does not solve the theological tension (prior doctrine vs. new doctrine); it suppresses it by declaring the new doctrine binding and treating dissent as apostasy. This is a classic mandatrophy case: the institution's mandate (coherent, authoritative doctrine) depends on the constraint being binding, so the constraint persists even though the underlying justification (revelation) is contested. The theater_ratio trajectory (starting at 0.72, declining to 0.58) reflects this: high theater initially (extensive apologetics and reaffirmations that this is revelation) indicates the constraint's legitimacy is unstable and requires performative maintenance; declining theater suggests legitimacy is settling (acceptance increases, apologetics decrease), but the underlying contestation remains suppressed rather than resolved. The classification as tangled_rope (not snare) depends on accepting the substitutionist reading's framing (Manifesto is revelation, not capitulation). A different reading (coercion_visibility or continuationist) would classify differently. The engine's per-seat computation will capture this: institutional_leadership seats will compute closer to rope; victim seats will compute closer to snare. The story's authored claim (tangled_rope) aligns with the institutional_leadership reading; the engine's seat-specific calculations will reveal the divergence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_capitulation,
    'Is the Manifesto a genuine new revelation from the divine authority the institutional tradition recognizes, or is it institutional capitulation to federal coercion, retroactively framed as theological development?',
    'This omega is irreducibly theological: no empirical measurement can distinguish a genuine revelation from an institutionally-motivated reframing, because both would produce identical observable facts (a new doctrine declared with authority and enforced within the institution). The question divides the tradition itself: the substitutionist reading asserts genuine revelation; the continuationist and coercion_visibility readings assert capitulation. Resolution would require access to the divine authority''s intentions, which is outside empirical reach.',
    'If the Manifesto is a genuine revelation, the monogamy requirement is binding doctrine and practitioners of historical polygamy are apostate; the constraint is a rope (legitimate coordination around new doctrine). If it is capitulation, the prior polygamy doctrine remains binding and the Manifesto is an illegitimate exercise of authority; the constraint is a snare (extraction justified by a false legitimacy claim). The classification depends entirely on this omega''s resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_vs_capitulation, conceptual, 'The irreducible theological question: whether the Manifesto instantiates a new divine command or institutional capitulation to coercion, framed as revelation.').

omega_variable(
    doctrinal_authority_chain,
    'Does institutional leadership hold the authority to declare a new revelation that supersedes a prior revelation, or is such authority limited to the original revelatory source (which cannot be invoked or redeclared by human agents)?',
    'This question is internal to the tradition''s own theological framework. The substitutionist reading asserts that institutional leadership is the authorized channel for new revelation; continuationist and coercion_visibility readings assert the prior revelation is binding and cannot be superseded by human institutional declaration. This divides not on empirical facts but on theological premises about the nature of revelatory authority.',
    'If leadership can declare binding revisions to prior doctrine, the Manifesto''s authority is legitimate and the monogamy requirement is binding. If it cannot, the Manifesto is an unauthorized departure from prior binding doctrine and practitioners of historical polygamy remain in doctrinal compliance. This determines whether the constraint is enforced legitimate authority (rope) or coercive illegitimate imposition (snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_authority_chain, conceptual, 'Whether institutional leadership possesses the authority to supersede prior doctrine via new revelation.').

omega_variable(
    suppression_mechanism_identity_fusion,
    'Is the measured suppression (0.72) primarily structural (legal penalties, institutional exclusion, economic dependency on the institution) or primarily internalized (practitioners'' identity fusion with the institution making exit psychologically prohibitive despite nominal availability)?',
    'Post-exit trajectory of former practitioners: if suppression persists after institutional exit (former members continue to experience belief/identity coherence crises, social isolation, economic hardship due to severed community ties), then suppression is substantially internalized and identity-fused. If suppression dissipates after exit, it is primarily structural.',
    'If suppression is internalized, the constraint''s effective suppression is higher than the structural measure suggests—victims carry the suppression with them and remain functionally trapped even after nominal exit. If structural, the 0.72 adequately captures the active enforcement required to maintain the constraint. Identity-fused targets are harder to mobilize for resistance (higher individual-level accessibility_collapse) and present as higher-theater because the suppression is partially self-administered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_fusion, empirical, 'Whether suppression is primarily structural coercion or primarily internalized identity fusion.').

omega_variable(
    reading_institutional_status,
    'Is the substitutionist_reading (monogamy is now doctrinally required; Manifesto is new revelation) maintained as bindable institutional doctrine by organizational leadership and accepted by a dominant portion of membership, or is it contested by significant factions within the tradition itself?',
    'Institutional membership surveys, schism history (FLDS vs. mainstream organizational split, doctrinal disputes within successor organizations), and documented public dispute records over whether the Manifesto constitutes binding revelation or prudential accommodation.',
    'If substitutionism is the dominant, accepted reading within institutional leadership and a substantial portion of membership, the constraint''s legitimacy is relatively settled and the classification stabilizes as tangled_rope. If it is heavily contested (significant factions maintain continuationism or coercion_visibility readings), the constraint is effectively a snare whose legitimacy is perpetually under assault; institutional leadership must expend suppression to maintain it. The theater_ratio trajectory (starting at 0.72, declining to 0.58) suggests the suppression required is declining as the reading becomes institutionalized—initial high theater (extensive doctrinal apologetics, repeated reaffirmations of revelation status) declines as acceptance settles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_institutional_status, empirical, 'Whether the substitutionist reading maintains bindable legitimacy within the tradition''s own framework or remains a contested, imposed reading requiring ongoing enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__substitutionist_reading, theater_ratio, 0, 0.72).
narrative_ontology:measurement_basis(divi_tr_t0, observed).
narrative_ontology:measurement(divi_tr_t5, divine_marriage_command__substitutionist_reading, theater_ratio, 5, 0.68).
narrative_ontology:measurement_basis(divi_tr_t5, observed).
narrative_ontology:measurement(divi_tr_t10, divine_marriage_command__substitutionist_reading, theater_ratio, 10, 0.65).
narrative_ontology:measurement_basis(divi_tr_t10, observed).
narrative_ontology:measurement(divi_tr_t15, divine_marriage_command__substitutionist_reading, theater_ratio, 15, 0.62).
narrative_ontology:measurement_basis(divi_tr_t15, observed).
narrative_ontology:measurement(divi_tr_t20, divine_marriage_command__substitutionist_reading, theater_ratio, 20, 0.6).
narrative_ontology:measurement_basis(divi_tr_t20, observed).
narrative_ontology:measurement(divi_tr_t25, divine_marriage_command__substitutionist_reading, theater_ratio, 25, 0.59).
narrative_ontology:measurement_basis(divi_tr_t25, observed).
narrative_ontology:measurement(divi_tr_t30, divine_marriage_command__substitutionist_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(divi_tr_t30, observed).
narrative_ontology:measurement(divi_tr_t40, divine_marriage_command__substitutionist_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(divi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__substitutionist_reading, base_extractiveness, 0, 0.31).
narrative_ontology:measurement_basis(divi_be_t0, projected).
narrative_ontology:measurement(divi_be_t5, divine_marriage_command__substitutionist_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(divi_be_t5, observed).
narrative_ontology:measurement(divi_be_t10, divine_marriage_command__substitutionist_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement_basis(divi_be_t10, observed).
narrative_ontology:measurement(divi_be_t15, divine_marriage_command__substitutionist_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(divi_be_t15, observed).
narrative_ontology:measurement(divi_be_t20, divine_marriage_command__substitutionist_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(divi_be_t20, observed).
narrative_ontology:measurement(divi_be_t25, divine_marriage_command__substitutionist_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(divi_be_t25, observed).
narrative_ontology:measurement(divi_be_t30, divine_marriage_command__substitutionist_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(divi_be_t30, observed).
narrative_ontology:measurement(divi_be_t40, divine_marriage_command__substitutionist_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(divi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__substitutionist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(divi_su_t0, observed).
narrative_ontology:measurement(divi_su_t5, divine_marriage_command__substitutionist_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(divi_su_t5, observed).
narrative_ontology:measurement(divi_su_t10, divine_marriage_command__substitutionist_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(divi_su_t10, observed).
narrative_ontology:measurement(divi_su_t15, divine_marriage_command__substitutionist_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(divi_su_t15, observed).
narrative_ontology:measurement(divi_su_t20, divine_marriage_command__substitutionist_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(divi_su_t20, observed).
narrative_ontology:measurement(divi_su_t25, divine_marriage_command__substitutionist_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(divi_su_t25, observed).
narrative_ontology:measurement(divi_su_t30, divine_marriage_command__substitutionist_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(divi_su_t30, observed).
narrative_ontology:measurement(divi_su_t40, divine_marriage_command__substitutionist_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(divi_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__substitutionist_reading, 0.12).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% The divine_marriage_command kernel generates three distinct constraint stories, one per reading: substitutionist_reading (this story) frames the Manifesto as new revelation and monogamy as binding doctrine; continuationist_reading frames the prior polygamy doctrine as binding and the Manifesto as prudential suspension; coercion_visibility_reading frames the Manifesto as institutional response to federal coercion. Each reading has its own ε (extractiveness), structural beneficiary/victim set, and classification. The three stories are linked via network.affects_constraints and share a kernel_id. The shared kernel (divine marriage command) persists across all three readings, but each reading instantiates a different constraint—different ε, different victims, different legitimacy claims. The divergence between readings reflects the kernel-level contestation: what the divine command requires cannot be adjudicated from within the tradition's own framework without begging the question of hermeneutical authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__substitutionist_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
