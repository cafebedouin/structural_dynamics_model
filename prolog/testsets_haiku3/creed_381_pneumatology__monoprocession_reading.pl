% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__monoprocession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creed_381_pneumatology__monoprocession_reading, []).

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
 *   constraint_id: creed_381_pneumatology__monoprocession_reading
 *   human_readable: Council of 381 Pneumatology: Monoprocession Reading (Spirit from Father Alone)
 *   domain: ecclesiastical/theological
 *
 * SUMMARY:
 *   This story instantiates the monoprocession reading of the contested
 *   kernel 'creed_381_pneumatology.' The kernel is the Council of 381's
 *   statement on the procession of the Spirit; the reading declares that this
 *   creed cannot be amended unilaterally and that any amendment without
 *   ecumenical consent constitutes a breach of communion. This reading has
 *   been held by Eastern Orthodox and Oriental Orthodox churches from the
 *   schism forward and is foundational to their pneumatological identity and
 *   their defense against Western doctrinal domination. The Western (Roman)
 *   See's unilateral adoption of the Filioque amendment (the Spirit proceeds
 *   from Father AND Son) is read by this reading as exactly the breach it
 *   forbids—a single see imposing doctrine on the whole Church without
 *   ecumenical consent. The monoprocession reading is structurally extractive
 *   of Western unilateral power and protective of Eastern collective
 *   authority; it extracts by blocking Western innovation and suppresses
 *   alternatives (reunion pluralism, Filioque legitimation) by treating the
 *   381 creed as inviolable universal law rather than negotiable regional
 *   expression. The constraint is a tangled rope: genuine coordination
 *   function (decentralized authority, protection against papal domination)
 *   coupled with asymmetric extraction (Eastern benefit, Western cost).
 *
 * KEY AGENTS:
 *   - Eastern autocephalous churches: primary beneficiary; holds Eastern pneumatological identity and structural protection against unilateral Western amendment; agenda-setter through councils and magisterium
 *   - Western unilateral innovators (Roman See and allied theologians): primary payer; blocked from unilateral doctrinal development; branded as breaching communion when innovation is adopted without ecumenical consent
 *   - Ecumenical councils institution: agenda-setter; enforces the reading by withholding recognition of unilateral amendments and declaring innovations outside catholicity
 *   - Reunion advocates: excluded; their solution (regional pluralism) requires abandoning the reading's core (inviolable universal creed)
 *   - Individual Western theologians: payer; constrained from advocating monoprocession or unity without institutional cost
 *   - Historical analyst: observer; documents constraint operation and consequences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__monoprocession_reading, 0.68).
domain_priors:suppression_score(creed_381_pneumatology__monoprocession_reading, 0.45).
domain_priors:theater_ratio(creed_381_pneumatology__monoprocession_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__monoprocession_reading, tangled_rope).
narrative_ontology:human_readable(creed_381_pneumatology__monoprocession_reading, "Council of 381 Pneumatology: Monoprocession Reading (Spirit from Father Alone)").
narrative_ontology:topic_domain(creed_381_pneumatology__monoprocession_reading, "ecclesiastical/theological").

domain_priors:requires_active_enforcement(creed_381_pneumatology__monoprocession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__monoprocession_reading, '178ee79a-9d9a-48a9-960e-c39d83f839bb').
narrative_ontology:cs_kernel_codification('178ee79a-9d9a-48a9-960e-c39d83f839bb', fixed_text).
narrative_ontology:cs_authority_grounding('178ee79a-9d9a-48a9-960e-c39d83f839bb', lineage).
narrative_ontology:cs_interpretation_layer_present('178ee79a-9d9a-48a9-960e-c39d83f839bb').
narrative_ontology:cs_reading_relation('178ee79a-9d9a-48a9-960e-c39d83f839bb', creed_381_pneumatology__filioque_reading, forecloses).
narrative_ontology:cs_reading_relation('178ee79a-9d9a-48a9-960e-c39d83f839bb', creed_381_pneumatology__ecumenical_reunion_reading, coexists_with).
narrative_ontology:cs_axiom('178ee79a-9d9a-48a9-960e-c39d83f839bb', foundational, creed_381_inviolable_universal_dogma).
narrative_ontology:cs_axiom_status(creed_381_inviolable_universal_dogma, holdable).
narrative_ontology:cs_axiom_grounding('178ee79a-9d9a-48a9-960e-c39d83f839bb', creed_381_inviolable_universal_dogma, deontological).
narrative_ontology:cs_axiom('178ee79a-9d9a-48a9-960e-c39d83f839bb', foundational, unilateral_amendment_constitutes_breach_of_communion).
narrative_ontology:cs_axiom_status(unilateral_amendment_constitutes_breach_of_communion, holdable).
narrative_ontology:cs_axiom_grounding('178ee79a-9d9a-48a9-960e-c39d83f839bb', unilateral_amendment_constitutes_breach_of_communion, deontological).
narrative_ontology:cs_axiom('178ee79a-9d9a-48a9-960e-c39d83f839bb', secondary, decentralized_conciliar_authority_protects_catholicity).
narrative_ontology:cs_axiom_status(decentralized_conciliar_authority_protects_catholicity, holdable).
narrative_ontology:cs_axiom_grounding('178ee79a-9d9a-48a9-960e-c39d83f839bb', decentralized_conciliar_authority_protects_catholicity, conventional).
narrative_ontology:cs_reference_frame('178ee79a-9d9a-48a9-960e-c39d83f839bb', conciliar_collegial_authority_structure).
narrative_ontology:cs_drift_state('178ee79a-9d9a-48a9-960e-c39d83f839bb', contemporary_reunion_ecumenism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('178ee79a-9d9a-48a9-960e-c39d83f839bb', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches).
narrative_ontology:constraint_victim(creed_381_pneumatology__monoprocession_reading, western_unilateral_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(creed_381_pneumatology__monoprocession_reading, individual_theologians_western).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Orthodox and Oriental Orthodox autocephalous churches hold to the monoprocession reading as foundational to their christological and pneumatological identity. Under this reading, the 381 creed is inviolable dogma; no unilateral Western amendment can bind the Church without ecumenical consent. They administer the constraint through their own magisterial authority and councils, and they benefit from the protection it provides against Western doctrinal domination.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches, beneficiary,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches, agenda_setter).

% The Western (Roman) See and theologians who endorse the Filioque amendment pay the cost of this constraint: they are blocked from unilaterally legislating doctrinal development for the whole Church, and their innovation is branded a breach of communion when adopted without ecumenical consent. They bear the cost of schism and the accusation of heresy from those who hold the monoprocession reading.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, western_unilateral_innovators, payer,
    institutional, civilizational, constrained, global).

% The institution of ecumenical councils (convened by consensus of sees, ideally) holds the authority to interpret and amend the creed. Under the monoprocession reading, this institution is the only legitimate body that can alter the 381 creed. It enforces the reading by withholding recognition of unilateral amendments and by declaring single-see innovations outside the bounds of catholicity.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, ecumenical_councils_institution, agenda_setter,
    institutional, civilizational, analytical, universal).

% Modern ecumenical and reunion movements that propose allowing both monoprocession and Filioque as legitimate regional expressions are structurally excluded from this reading's framework. Their solution (bilateral acceptance, regional pluralism) requires abandoning the monoprocession reading's core: that the 381 creed is inviolable universal doctrine, not a regional expression.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, reunion_advocates, excluded,
    organized, biographical, constrained, global).

% Western theologians who personally endorse the monoprocession reading or wish to advocate for doctrinal unity on its terms face institutional and career constraints: they must either defend the Western See's institutional position (the Filioque) or break communion with their own magisterium, a cost that includes loss of institutional standing and access to teaching positions.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, individual_theologians_western, payer,
    moderate, biographical, constrained, national).

% A historical or theological scholar examining the constraint's operation across time and institutions, taking no institutional position but documenting how the monoprocession reading has been enforced, how alternative readings have been suppressed, and what the structural consequences have been for Church unity and doctrinal authority.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, historical_analyst, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches).
narrative_ontology:fixing_cost_class(creed_381_pneumatology__monoprocession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a decentralized authority structure for the whole Church: no single see (Pope or patriarch) may unilaterally define Trinitarian doctrine binding on all communions. Coordinates the sees around a shared dogma (the 381 creed) and a shared decision-making process (ecumenical councils) rather than submitting all to a single authority.
% TRANSFER_FUNCTION: Transfers doctrinal authority from unilateral Western innovation (the Filioque amendment, adopted without ecumenical consent) back to the collective sees. Western theological precision must pass through ecumenical consensus or forfeit universal binding force. Innovation-asserting power is constrained in favor of council-mediated change.
% ABSENT_VOICES: Western individual theologians and reunion advocates who endorse regional pluralism (both Filioque and monoprocession as legitimate expressions) are excluded from the monoprocession reading's framework. They would argue for theological flexibility and local doctrinal autonomy; the reading keeps them outside by treating the 381 creed as inviolable universal doctrine, not negotiable regional preference.
% DISAPPEARANCE_RATIONALE: If this constraint—the rule that the 381 creed cannot be amended unilaterally and that amendment without ecumenical consent constitutes breach—vanished, the structure of Church authority would reorganize. The Western See would regain unilateral magisterial authority to define doctrine; Eastern autocephalous churches would lose their structural protection against doctrinal imposition; councils would become consultative rather than authoritative. The decentralized polity would collapse into a centralized one.
% FOUNDING_PROBLEM: After the First Council of Constantinople (381 CE), the Church faced the question: who holds authority to develop and clarify Trinitarian doctrine? Is it the whole Church in ecumenical assembly, or can a single see (the Western patriarchate, increasingly centralized) unilaterally legislate development of dogma? The monoprocession reading asserts the founding problem is the prevention of unilateral doctrinal domination by any single see, preserving the collegial authority structure.
% FOUNDING_PROBLEM_CORROBORATION: Eastern Orthodox theologians and church fathers attest the founding problem is live: Western unilateral innovation (the Filioque) demonstrates that without the constraint, a single see will impose doctrine on others. Western Catholic theologians and magisterial sources attest the founding problem is solved or mischaracterized: legitimate doctrinal development is the Pope's proper function, and the Filioque represents implicit truth already present in 381, not innovation. Neither party speaks only for itself: Eastern sources cite the theology of the ecumenical councils and the fathers; Western sources cite medieval theology and papal statements; reunion theologians cite modern ecumenical texts. The contest is not resolved by institutional authority alone.
narrative_ontology:disappearance_verdict(creed_381_pneumatology__monoprocession_reading, world_rearranges).
narrative_ontology:founding_problem_status(creed_381_pneumatology__monoprocession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(creed_381_pneumatology__monoprocession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(creed_381_pneumatology__monoprocession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(creed_381_pneumatology__monoprocession_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creed_381_pneumatology__monoprocession_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(creed_381_pneumatology__monoprocession_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(creed_381_pneumatology__monoprocession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint blocks Western unilateral authority and transfers doctrinal power to the collective sees. This is not extraction in the predatory sense—it is power redistribution that protects a decentralized polity structure—but it IS extractive from the Western institutional position that assumed unilateral magisterial authority. Suppression is moderate (0.45) because enforcement does not require violent coercion; it operates through doctrinal anathema, ecclesial exclusion, and magisterial non-recognition. Reunion advocates are suppressed structurally (their framework is incompatible with the monoprocession reading's premise of inviolable dogma) rather than through force. Theater is low-moderate (0.22): the constraint's function is real (preventing unilateral doctrinal domination), but enforcement includes performative restatement of the inviolability of 381 when Western innovation threatens. The measurement series tracks the constraint's intensity over the long interval from 381 to 2026. Extractiveness rises sharply from 381 to 1054 (the Great Schism, when the constraint begins to be enforced), peaks at 1500 (the Reformation, maximum Western/Eastern polarization), and stabilizes at a high level through the modern period. Theater rises gradually as enforcement becomes increasingly institutional and declarative (councils reaffirming 381, councils condemning unilateral innovation) rather than grassroots conviction. Suppression follows a similar arc, rising as the constraint's enforcement becomes institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (Western innovators and individual Western theologians) and the beneficiary seats (Eastern churches) should compute very differently. From the Eastern beneficiary perspective, the constraint is a legitimate safeguard against doctrinal imperialism and a true coordination mechanism for Church unity on collegial terms. From the Western payer perspective, the constraint is a veto by Eastern intransigence on legitimate development of implicit dogma, and unilateral adoption of the Filioque is not innovation but clarification of what was always implicit. The engine computes these perspectives from the authored structural data (power, exit, beneficiary/victim declarations) and produces different classification outcomes for the same constraint from different seats. This divergence is the analytic content the Deferential Realism framework is designed to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Eastern autocephalous churches experience this constraint as beneficiary: they benefit from the protection it provides against Western doctrinal imposition, and they hold agenda-setting power through councils and magisterium. Their directionality (d) is near 0.0 (full beneficiary side). Western unilateral innovators experience it as payer: they bear the cost of blocked innovation, institutional anathema, and loss of magisterial reach over the Eastern churches. Their directionality is near 1.0 (full target side). Ecumenical councils sit near the agenda-setter position: they enforce the reading and benefit from their central authority role, though they also carry the burden of convening consensus across dispersed sees. Reunion advocates are excluded from the framework—they would experience the constraint as victim (their theology is declared illegitimate), but the reading does not author them as payers because it does not engage their alternative; it simply excludes it. Individual Western theologians are payers with constrained exit (identity-locked): advocating the monoprocession reading requires breaking from their own magisterium, a cost that binds them to the Western See's institutional position even when they privately sympathize with the Eastern position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—prevention of unilateral doctrinal domination by any single see—remains live and contested. The Eastern churches attest it is live (Western unilateralism is still a threat); Western sources attest it is solved or mischaracterized (papal magisterium is proper authority). The constraint persists not because the founding problem is solved but because neither party has the power to impose a solution unilaterally. The constraint is sustained by structural stalemate (decentralized polity prevents Western domination; Eastern councils prevent Western unilateral amendment) rather than by agreement on the problem's resolution. This is the mark of a tangled_rope under mandatrophic pressure: genuine coordination function (decentralized authority) coupled with asymmetric extraction (power transfer from West to East) sustained by enforcement (anathema, exclusion) in the absence of shared verdict on whether the original problem is live. The resurrection of ecumenical reunion movements in the 20th century is a late-stage symptom: the constraint is starting to be experienced by Western institutional actors as unsustainable (reunion would require abandoning either the Filioque or unilateral authority, neither palatable to Rome), and by Eastern actors as protection against an ever-present threat. The constraint persists despite mandatrophic pressure because the infrastructure that enforces it (councils, magisterial authority, doctrinal tradition) is deeply embedded in both Eastern and Western ecclesiastical identity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_vs_innovative_development,
    'Does the Filioque represent implicit development of doctrine already present in 381 (clarification), or does it represent new doctrine (innovation)?',
    'Systematic textual analysis of the Cappadocian fathers and 381 council proceedings against medieval and modern theological scholarship. Historical reconstruction of how the Filioque emerged in the Western liturgy and when it acquired dogmatic status.',
    'If implicit: Western development is legitimate refinement of 381, and the monoprocession reading''s claim that amendment requires ecumenical consent becomes a procedural gate on legitimate development rather than a substantive protection of unchanging dogma. If innovative: the Filioque is exactly the unilateral breach the constraint forbids.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implicit_vs_innovative_development, empirical, 'Whether Filioque is doctrinal development or unilateral innovation.').

omega_variable(
    decentralized_vs_papal_authority,
    'Is decentralized, collegial authority (councils representing dispersed sees) structurally necessary for legitimate Church governance, or can papal magisterium exercise legitimate unilateral authority in defining doctrine?',
    'Theological exegesis of scriptural passages on Peter, councils, and magisterial authority; historical analysis of authority structures in the early Church; phenomenological study of which authority structure produces better pastoral and doctrinal outcomes.',
    'If decentralization is necessary: the constraint is a legitimate safeguard of proper Church structure. If papal authority is legitimate: the constraint is an illegitimate veto on proper magisterial function. This is the foundational axiom dispute between the readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decentralized_vs_papal_authority, conceptual, 'Whether Church governance is properly decentralized or papal.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of reunion pluralism and filioque advocacy in the Eastern tradition structural (enforced through anathema and institutional exclusion) or internalized (Eastern theologians believe monoprocession is true, not merely enforced)?',
    'Post-schism analysis of Eastern theological freedom and internal debate; comparative study of how Eastern theologians privately discuss and teach Filioque vs. how they present it publicly; historical cases where Eastern theologians have departed from official teaching.',
    'If structural: the constraint''s suppression is held in place by institutional enforcement and would weaken if enforcement relaxed. If internalized: the constraint is self-reinforcing through deep theological conviction and would persist even without enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternative pneumatologies is structural or internalized in Eastern tradition.').

omega_variable(
    council_authority_source,
    'What is the source and legitimacy of ecumenical council authority? Is it delegated from Christ through apostolic succession (Eastern view), or is it a function of papal authorization (Western view)?',
    'Exegetical and historical analysis of council authority in the first four councils; systematic comparison of conciliar self-descriptions across East and West; theological analysis of how each tradition grounds council legitimacy.',
    'This is the foundational commitment-system dispute: the reading''s core assertion that councils (not a single see) hold binding authority rests on a particular answer to this question. The answer distinguishes this reading from the filioque reading, which locates authority differently.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(council_authority_source, conceptual, 'Source and legitimacy of ecumenical council authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__monoprocession_reading, 381, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cree_tr_t381, creed_381_pneumatology__monoprocession_reading, theater_ratio, 381, 0.05).
narrative_ontology:measurement_basis(cree_tr_t381, observed).
narrative_ontology:measurement(cree_tr_t800, creed_381_pneumatology__monoprocession_reading, theater_ratio, 800, 0.1).
narrative_ontology:measurement_basis(cree_tr_t800, observed).
narrative_ontology:measurement(cree_tr_t1054, creed_381_pneumatology__monoprocession_reading, theater_ratio, 1054, 0.15).
narrative_ontology:measurement_basis(cree_tr_t1054, observed).
narrative_ontology:measurement(cree_tr_t1500, creed_381_pneumatology__monoprocession_reading, theater_ratio, 1500, 0.22).
narrative_ontology:measurement_basis(cree_tr_t1500, observed).
narrative_ontology:measurement(cree_tr_t1900, creed_381_pneumatology__monoprocession_reading, theater_ratio, 1900, 0.25).
narrative_ontology:measurement_basis(cree_tr_t1900, observed).
narrative_ontology:measurement(cree_tr_t2026, creed_381_pneumatology__monoprocession_reading, theater_ratio, 2026, 0.22).
narrative_ontology:measurement_basis(cree_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(cree_be_t381, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 381, 0.35).
narrative_ontology:measurement_basis(cree_be_t381, observed).
narrative_ontology:measurement(cree_be_t800, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 800, 0.52).
narrative_ontology:measurement_basis(cree_be_t800, observed).
narrative_ontology:measurement(cree_be_t1054, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 1054, 0.68).
narrative_ontology:measurement_basis(cree_be_t1054, observed).
narrative_ontology:measurement(cree_be_t1500, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 1500, 0.71).
narrative_ontology:measurement_basis(cree_be_t1500, observed).
narrative_ontology:measurement(cree_be_t1900, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 1900, 0.65).
narrative_ontology:measurement_basis(cree_be_t1900, observed).
narrative_ontology:measurement(cree_be_t2026, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(cree_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(cree_su_t381, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 381, 0.15).
narrative_ontology:measurement_basis(cree_su_t381, observed).
narrative_ontology:measurement(cree_su_t800, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 800, 0.28).
narrative_ontology:measurement_basis(cree_su_t800, observed).
narrative_ontology:measurement(cree_su_t1054, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 1054, 0.42).
narrative_ontology:measurement_basis(cree_su_t1054, observed).
narrative_ontology:measurement(cree_su_t1500, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 1500, 0.48).
narrative_ontology:measurement_basis(cree_su_t1500, observed).
narrative_ontology:measurement(cree_su_t1900, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 1900, 0.43).
narrative_ontology:measurement_basis(cree_su_t1900, observed).
narrative_ontology:measurement(cree_su_t2026, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 2026, 0.45).
narrative_ontology:measurement_basis(cree_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__monoprocession_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(creed_381_pneumatology__monoprocession_reading, 0.12).
narrative_ontology:affects_constraint(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology__filioque_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology__ecumenical_reunion_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of kernel creed_381_pneumatology; its siblings are filioque_reading and ecumenical_reunion_reading. The three constraints share the kernel text (the 381 creed) but differ in how they interpret the creed's authority and amendability. This story (monoprocession_reading) asserts the creed is inviolable universal dogma; filioque_reading asserts papal magisterium can develop it; ecumenical_reunion_reading asserts both can coexist as regional expressions. The ε values differ sharply: monoprocession_reading is high-ε (structurally extractive of Western power) because it protects decentralized authority; filioque_reading is lower-ε (if papal development is legitimate, less extraction is occurring); ecumenical_reunion_reading is moderate-ε (accepts Western innovation but requires bilateral consent, a compromise structure). These are not the same constraint viewed differently; they are three different constraints that arise from readings of the same kernel. The constraint family is completed when all three are authored.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(creed_381_pneumatology__monoprocession_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
