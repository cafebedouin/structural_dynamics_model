% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__prophetic_override_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__prophetic_override_reading
 *   human_readable: Continuing Revelation Override of Eternal Covenant (Prophetic Authority Reading)
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   The Church of Jesus Christ of Latter-day Saints taught eternal marriage
 *   (sealing) and polygamy as divine imperatives established through Joseph
 *   Smith and required for exaltation. Federal pressure (territorial
 *   governance restrictions, property seizure, criminal prosecution) mounted
 *   against the practice throughout the late 19th century. In 1890, church
 *   president Wilford Woodruff issued the Manifesto, declaring that polygamy
 *   was being suspended in obedience to federal law. The manifesto was framed
 *   using continuing revelation doctrine: the living prophet had received new
 *   revelation permitting him to supersede the prior revelation that
 *   commanded polygamy. This reading instantiates the continuing revelation
 *   framing—the doctrine that allows the prophet to override prior revelation
 *   when circumstances (here, federal political pressure) require it. The
 *   constraint measured is not the doctrine of eternal marriage itself, but
 *   the doctrine of prophetic authority to override it.
 *
 * KEY AGENTS:
 *   - Living Prophet/Prophetic Authority: institutional position with claimed access to continuing revelation; benefits from maintaining institutional survival and regulatory compliance
 *   - Polygamy-practicing believers: those covenanted to plural marriage who now receive revelation superseding their covenant obligation; bear the extraction of covenant dissolution
 *   - Women in polygamous unions: structurally dependent on the covenant's continuation; face dissolution and status loss
 *   - Federal government: applies external pressure that activates the prophetic override doctrine
 *   - Church institutional hierarchy: benefits from the doctrine's ability to reconcile covenant practice with state pressure without renouncing doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.68).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.72).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Continuing Revelation Override of Eternal Covenant (Prophetic Authority Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, '91dbc4da-2510-4692-baf7-1a8b6a8df260').
narrative_ontology:cs_kernel_codification('91dbc4da-2510-4692-baf7-1a8b6a8df260', fixed_text).
narrative_ontology:cs_authority_grounding('91dbc4da-2510-4692-baf7-1a8b6a8df260', extraction).
narrative_ontology:cs_interpretation_layer_present('91dbc4da-2510-4692-baf7-1a8b6a8df260').
narrative_ontology:cs_reading_relation('91dbc4da-2510-4692-baf7-1a8b6a8df260', eternal_marriage_covenant__immutable_commandment_reading, forecloses).
narrative_ontology:cs_reading_relation('91dbc4da-2510-4692-baf7-1a8b6a8df260', eternal_marriage_covenant__temporal_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('91dbc4da-2510-4692-baf7-1a8b6a8df260', foundational, living_prophet_continues_revelation).
narrative_ontology:cs_axiom_status(living_prophet_continues_revelation, holdable).
narrative_ontology:cs_axiom_grounding('91dbc4da-2510-4692-baf7-1a8b6a8df260', living_prophet_continues_revelation, theological).
narrative_ontology:cs_axiom('91dbc4da-2510-4692-baf7-1a8b6a8df260', foundational, prophetic_override_supersedes_prior_doctrine).
narrative_ontology:cs_axiom_status(prophetic_override_supersedes_prior_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('91dbc4da-2510-4692-baf7-1a8b6a8df260', prophetic_override_supersedes_prior_doctrine, deontological).
narrative_ontology:cs_reference_frame('91dbc4da-2510-4692-baf7-1a8b6a8df260', prophetic_authority_as_doctrine_source).
narrative_ontology:cs_drift_state('91dbc4da-2510-4692-baf7-1a8b6a8df260', federal_pressure_escalation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('91dbc4da-2510-4692-baf7-1a8b6a8df260', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, prophetic_authority_institution).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_institutional_survival).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, covenant_believers_practicing_polygamy).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, women_in_polygamous_unions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, women_in_polygamous_unions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional position claiming authority to receive continuing revelation on behalf of the church. The living prophet receives and announces new revelations, including the Manifesto. Benefits from the doctrine because it permits unilateral reinterpretation of prior commitments without losing authority legitimacy; in fact, the doctrine elevates prophetic authority by demonstrating its power to override even eternal covenants. Sets the agenda for doctrine and practice; enforcement of new revelation falls to the church hierarchy. Cannot exit the doctrine without renouncing the legitimacy basis of prophetic authority itself.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, prophetic_authority_institution, agenda_setter,
    institutional, civilizational, analytical, global).

% The church as an institutional entity faces federal pressure (territorial restrictions, property seizure, criminal prosecution) that threatens its legal existence and territorial legitimacy. The continuing revelation doctrine permits the institution to adapt to external pressure (suspending polygamy practice) without surrendering the theological claim (the doctrine remains eternally valid), preserving institutional viability. This is not a person or an agent; it is a non-agent entity (the organization-as-persisting-entity). Included because the institutional survival narrative is central to how the doctrine's beneficiaries justify its deployment.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_institutional_survival, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(eternal_marriage_covenant__prophetic_override_reading, church_institutional_survival).

% Believers who accepted and acted on the prior revelation commanding polygamy as essential for exaltation. They structured their families, identities, and spiritual commitments around this covenant. The Manifesto, justified through continuing revelation doctrine, requires them to abandon polygamous practice and reinterpret their covenants. They face institutional discipline if they resist (excommunication, loss of temple privileges, social exclusion), federal legal pressure (prosecution for polygamy), and the internalized conviction that rejecting the prophet's revelation is apostasy. Their exit options are extremely constrained: leaving the faith entirely means repudiating their entire identity structure; staying means accepting the override; practicing polygamy means criminality and institutional excommunication. Identity-locked because the covenant practice was integrated into their spiritual identity and sense of required exaltation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, covenant_believers_practicing_polygamy, payer,
    powerless, biographical, identity_locked, national).

% Women covenanted in plural marriages face particular extraction: they lose the legal standing and economic security that polygamy provided (multiple male earners, distributed household labor, plural partnership). The Manifesto dissolution of polygamy leaves them in a complex situation: some are left without formal marriage status (the subordinate wives in plural unions), some face economic precarity, and all must reconcile their covenant identity with the institutional override. Federal pressure actually provides some relief (prohibition of plural marriage shifts toward legal protection of women), but the continuing revelation doctrine frames their situation as divinely ordained override rather than legal protection—they must accept loss of covenant status as spiritually correct. Identity-locked by religious identity and economic dependence on the polygamous family structure.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, women_in_polygamous_unions, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__prophetic_override_reading, women_in_polygamous_unions, beneficiary).

% Federal authorities apply pressure through territorial governance restrictions, property seizure, criminal prosecution, and refusal to admit Utah to statehood without polygamy suppression. The federal pressure is the structural trigger that activates the continuing revelation doctrine as the church's adaptive response. The observer position reflects the federal role as external force shaping the church's doctrine, without being a stakeholder in the doctrine's internal legitimacy claims.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, federal_government, observer,
    powerful, biographical, analytical, national).

% Believers who reject the Manifesto's authority and continue to practice polygamy in defiance of the institutional override. They are excluded from official church structures (excommunicated), from temple participation, and from institutional voice. They would argue that the Manifesto violates the eternal covenant and that the doctrine of continuing revelation is being misused to rationalize capitulation to federal pressure. Their absence from institutional decision-making is the absence the constraint relies on—if they had institutional voice, the override would be contestable.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, dissenting_believers, excluded,
    powerless, biographical, trapped, regional).

% External analysis seat: examines the structure of prophetic authority, the doctrine of continuing revelation, and the evidence for whether the doctrine functions as genuine revelation mechanism or as institutional rationalization. Does not participate in the authority structure or the covenant commitment; reads the constraint from outside the tradition.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__prophetic_override_reading, prophetic_authority_institution).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__prophetic_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permits the institutional prophet to navigate external political pressure (federal suppression of polygamy) without renouncing the institutional authority structure itself. The doctrine of continuing revelation allows the prophet to revise prior doctrine in response to changed circumstances, preserving both institutional viability and prophetic authority as the ultimate source of legitimacy.
% TRANSFER_FUNCTION: Moves the cost of institutional adaptation onto believers practicing polygamy and women in plural unions. They surrender the right to practice their covenant, accept loss of status and sometimes economic security, and internalize the narrative that this surrender is spiritually correct because it comes through the prophet. The institutional prophet and the church hierarchy gain unilateral power to reshape covenant commitments without losing legitimacy.
% ABSENT_VOICES: Dissenting believers who reject the Manifesto and view it as capitulation to federal pressure rather than divine revelation. They are excommunicated and institutionally silenced; their position—that the covenant is eternally binding and cannot be overridden—would challenge the continuing revelation doctrine's legitimacy if they retained institutional voice. Also absent: historical polygamy-practicing communities whose cultural practices are erased by the institutional framing.
% DISAPPEARANCE_RATIONALE: If continuing revelation doctrine and the prophetic override mechanism vanished, the church would face an institutional crisis: the prior revelation commanding polygamy would remain in the canon without an authorized override, forcing either explicit rejection of that revelation (undercutting scriptural authority) or continued defense of polygamy (undercutting federal legitimacy and territorial admission). The institutional arrangements of polygamous families would persist longer or reorganize differently if the institutional authority to override were removed. Contemporary church hierarchy and believers organized around the prophetic authority structure would lose the mechanism by which that authority performs unilateral reinterpretation.
% FOUNDING_PROBLEM: Federal government pressure threatens the church's institutional survival, legal status, and territorial presence. The church cannot defend polygamy practice against federal law without losing statehood admission and facing escalating prosecution. The founding problem is the collision between the institutional need to adapt to federal pressure and the theological claim that polygamy is eternally binding.
% FOUNDING_PROBLEM_CORROBORATION: Federal government records document the escalating pressure: territorial governance restrictions, property seizures, criminal prosecution, and statehood admission conditional on polygamy suppression. Church historians outside the institutional authority structure (e.g., non-believing or critical historians) confirm that federal pressure was the primary driver of the Manifesto and that the continuing revelation framing was deployed to make the adaptation appear theologically legitimate rather than politically coerced. Dissenting believers of the era (some of whom left records) explicitly argue that federal pressure, not divine revelation, drove the Manifesto. Contemporary historians note that the 'revelation' announced in 1890 is unusual in that it was not recorded until decades later and its contents remain contested.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__prophetic_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__prophetic_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eternal_marriage_covenant__prophetic_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__prophetic_override_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the doctrine transfers the cost of institutional adaptation entirely onto believers: those who accepted polygamy as eternal covenant must abandon it, reinterpret their covenants, and accept the prophet's override as legitimate—all while the doctrine itself (continuing revelation) remains unchallenged and even elevated as the source of the new authority. Suppression (0.72) is high because believers cannot contest the override: the doctrine places the prophet's revelatory claim beyond empirical verification or theological debate within the authority structure. Accessibility collapse (0.64) is substantial: once the doctrine is accepted (as it is by believers who recognize the prophet's authority), alternatives to accepting the override effectively vanish—the believer must either accept the revelation or repudiate the entire authority framework. Theater ratio (0.51) is at the threshold because the doctrine functions both to address a genuine institutional crisis (federal pressure that threatens the church's legal existence) and to consolidate prophetic authority by demonstrating the prophet's unilateral power to reshape covenant commitments. The measurement series show extractiveness and theater rising sharply as federal pressure mounts (t=0 to t=15) and then plateauing as the constraint becomes institutionalized (t=15 to t=25). Resistance (0.58) is moderate: significant opposition from believers who held polygamy as core identity, but insufficient to challenge the prophetic authority claim directly—resistance manifests as psychological distress and hidden practice rather than institutional challenge. The claim/metric divergence is deliberate: the church frames the constraint as rope (coordination through prophetic leadership), while the metrics describe a structure in which one party (the prophet's authority) unilaterally imposes a solution on another party (believers) and suppresses their ability to contest it—tangled rope.
 *
 * PERSPECTIVAL GAP:
 *   From the prophetic authority seat, the constraint is a legitimate coordination mechanism: the prophet receives new revelation appropriate to changed circumstances (federal pressure threatens church survival), and that revelation is binding because it comes from the authorized source. The extraction is minimal from this seat—it is the cost of institutional leadership and the divine will's adaptation. From the believer-payer seat, the constraint appears as asymmetric authority: believers accepted the prior revelation (polygamy is eternal and required) and organized their lives around it; now they are told that revelation was superseded without their consent or input, and they have no recourse to challenge the override. The suppression is structural and internalized: they cannot legally practice polygamy (structural suppression from the federal government) and the doctrine tells them that challenging the prophet's authority is apostasy (internalized suppression). The engine will compute divergent directionalities from these seats: the prophet sits at low d (beneficiary of the doctrine's power to reshape commitments), while believers sit at high d (targets whose covenants are overridden). This divergence is the measurement the framework takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Prophetic authority benefits from continuing revelation doctrine: it consolidates institutional power, permits adaptation to external pressure without renouncing prior claims, and places the prophet's decisions beyond challenge (the doctrine itself prevents contest). Beneficiary directionality is near 0.0. Believers practicing polygamy pay the extraction: they lose the right to their covenant practice, face social pressure and institutional discipline if they resist, and must reinterpret their entire spiritual commitment to align with the override. Target directionality is near 1.0. The constraint satisfies the Tangled Rope gate: it possesses a coordination function (the doctrine allows the institutional leader to navigate political crisis and maintain church legal status) and an extraction function (the doctrine shifts all institutional costs onto believers while consolidating prophetic authority). It requires active enforcement (the church hierarchy polices compliance, disciplines non-compliance, manages dissent).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving church institutional survival under federal pressure) is live at the time of the Manifesto and remains technically live in the constraint's interval (t=0 to t=25 covers the peak of federal pressure and the immediate aftermath). However, the theater ratio's rise to 0.51 suggests that by t=20-25, the doctrine is increasingly performing institutional authority rather than solving the actual survival crisis. The church's legal status stabilizes after t=10, yet the doctrine continues to be invoked to justify the polygamy suppression, suggesting that suppression persistence is partly inertial (continuing revelation doctrine is institutionalized as the mechanism of prophetic authority) and partly theatrical (the doctrine's power to override is rehearsed to validate the prophet's current decisions). The constraint is at risk of Piton classification if we extended the measurement interval: the doctrine's function shifts from solving an acute crisis (federal threat) to maintaining an institutional authority structure whose primary function has degraded. At the interval measured, it remains Tangled Rope, but the rising theater ratio is the early signal of that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divinely_ordained_vs_institutionally_convenient,
    'Does continuing revelation doctrine constitute a genuine theological mechanism for divine course-correction, or does it function primarily as institutional cover for policy reversals driven by external political pressure?',
    'Historical-textual analysis of revelation claims and their timing relative to federal pressure escalation; comparative study of other major doctrine revisions and their justifications; theological examination of how the doctrine is deployed in the authority structure''s own hermeneutics.',
    'If the former, the constraint is a legitimate (if asymmetric) coordination mechanism for divine guidance under changing circumstances; the extraction measured is the institutional cost of processing revelation. If the latter, the constraint is a snare whose cover story is theological, and the measured extraction reflects the gap between the doctrine''s stated function (divine will) and its actual function (institutional adaptation without accountability).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divinely_ordained_vs_institutionally_convenient, conceptual, 'Whether continuing revelation is a functional theological mechanism or institutional rationalization.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.72) primarily structural (federal legal apparatus, economic sanctions, property seizures) or internalized (believers genuinely absorb the narrative that obedience to federal law supersedes covenant obligation, such that exit remains suppressed even after structural barriers are removed)?',
    'Post-suppression trajectory: if believers exit polygamous practice after the Manifesto and report that the internalized narrative broke (revelation was institutional adaptation, not divine will), suppression was partly structural; if the narrative persists as binding even in absence of legal threat, suppression is internalized. Contemporary testimony from belief-transition cohorts.',
    'If suppression is primarily structural, removing federal pressure should permit rapid dissolution of the constraint; if internalized, the constraint persists as an identity-lock even when structural barriers are gone, indicating the doctrine has successfully rewritten believers'' self-conception of obedience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Structural vs. internalized suppression mechanism in polygamy abandonment.').

omega_variable(
    kernel_framing_authority_vs_revelation,
    'Does this reading locate authority in the prophet as receiver of genuine divine revelation (revelation is the kernel, authority is derivative), or in the prophet''s institutional position (authority is the kernel, revelation is the narrative deployed to legitimize authority''s decisions)?',
    'Textual archaeology of the revelation record and institutional authority structures; how the authority structure justifies reversals when empirical prophecy claims are later disputed or contradicted. Whether the doctrine permits non-prophets to challenge revelation on empirical grounds or whether institutional position determines revelation status.',
    'If revelation is the kernel, the constraint''s classification depends on whether the revelation mechanism is genuine (mountain-adjacent) or performative (snare-adjacent). If authority is the kernel, the constraint is necessarily a snare: the revelation narrative is the cover story for institutional power consolidation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_authority_vs_revelation, conceptual, 'Whether continuing revelation is the kernel or a narrative cover for institutional authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t0, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(eter_tr_t0, projected).
narrative_ontology:measurement(eter_tr_t5, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(eter_tr_t5, observed).
narrative_ontology:measurement(eter_tr_t10, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(eter_tr_t10, observed).
narrative_ontology:measurement(eter_tr_t15, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement_basis(eter_tr_t15, observed).
narrative_ontology:measurement(eter_tr_t20, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 20, 0.51).
narrative_ontology:measurement_basis(eter_tr_t20, observed).
narrative_ontology:measurement(eter_tr_t25, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 25, 0.51).
narrative_ontology:measurement_basis(eter_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(eter_be_t0, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(eter_be_t0, projected).
narrative_ontology:measurement(eter_be_t5, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(eter_be_t5, observed).
narrative_ontology:measurement(eter_be_t10, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement_basis(eter_be_t10, observed).
narrative_ontology:measurement(eter_be_t15, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(eter_be_t15, observed).
narrative_ontology:measurement(eter_be_t20, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(eter_be_t20, observed).
narrative_ontology:measurement(eter_be_t25, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(eter_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t0, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(eter_su_t0, projected).
narrative_ontology:measurement(eter_su_t5, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(eter_su_t5, observed).
narrative_ontology:measurement(eter_su_t10, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(eter_su_t10, observed).
narrative_ontology:measurement(eter_su_t15, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(eter_su_t15, observed).
narrative_ontology:measurement(eter_su_t20, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(eter_su_t20, observed).
narrative_ontology:measurement(eter_su_t25, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(eter_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__prophetic_override_reading, 0.12).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% The eternal_marriage_covenant kernel decomposes into three readings with structurally distinct ε values and beneficiary/victim sets. The prophetic_override_reading locates authority in the doctrine of continuing revelation (ε=0.68, high extraction, asymmetric enforcement). The immutable_commandment_reading locates authority in the eternal covenant itself as immutable law (different ε, different beneficiary set: those who believe polygamy is eternally required). The temporal_accommodation_reading treats the Manifesto as practice suspension without doctrine renunciation (different ε, different authority framing). Each reading is a constraint story; the three stories are linked by network.affects_constraints because they compete for legitimacy within the same kernel and within the same institutional tradition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eternal_marriage_covenant__prophetic_override_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
