% ============================================================================
% CONSTRAINT STORY: public_forum_doctrine__traditional_forum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_forum_doctrine__traditional_forum_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: public_forum_doctrine__traditional_forum_reading
 *   human_readable: Public Forum Doctrine — Traditional Forum Reading
 *   domain: constitutional_law/first_amendment/public_forums
 *
 * SUMMARY:
 *   The traditional forum reading of public forum doctrine grounds First
 *   Amendment protection in the historical status of streets and parks as
 *   commons held in trust for assembly and debate. This reading affirms that
 *   government property used immemorially for public gathering cannot be
 *   closed to disfavored speakers — the doctrine protects access as a
 *   coordinate public good, not as a privilege government grants or
 *   withholds. The reading conflicts with two sibling doctrinal frames: the
 *   designated forum reading (government can open forums and then close them,
 *   or never open them at all — the critical choice is whether to open, not
 *   whom to favor) and the government speech reading (government, as a
 *   speaker itself, can claim exemption from viewpoint neutrality in certain
 *   contexts like monuments and public programs). The constraint exhibits
 *   rope classification at the street-level speaker and movement scales
 *   (coordination without extraction), but piton classification at the
 *   institutional scale (degraded functional capacity maintained through
 *   continued judicial reaffirmation). The measurement trajectory shows
 *   increasing theater ratio as courts simultaneously reaffirm the doctrine
 *   in canonical form while granting exceptions that narrow its domain.
 *
 * KEY AGENTS:
 *   - Street-Level Speakers and Marchers: Primary beneficiary (powerless/mobile) — benefit from reliable access guarantee; no suppression of exit because doctrine prevents closure
 *   - Protest Movements: Secondary beneficiary (organized/constrained) — depend on doctrine's historical enforceability; scale-constrained but not blocked by doctrine
 *   - Municipal Authorities: Dual role (institutional/constrained) — coordinate through the doctrine (must maintain accessible commons) AND experience extraction (lose exclusive control); tangled rope perspective
 *   - Courts Enforcing Viewpoint Neutrality: Enforcement actor (analytical/analytical) — maintain the doctrine through case-by-case review; scaffold perspective because enforcement depends on continuous judicial commitment
 *   - Alternative Doctrinal Frames (government speech, designated forum): Competing authority (institutional/arbitrage) — erode the doctrine's domain through exception-creation; piton perspective observes this erosion
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks seeing the doctrine as immemorial natural law when it is actually a 20th-century construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_forum_doctrine__traditional_forum_reading, 0.28).
domain_priors:suppression_score(public_forum_doctrine__traditional_forum_reading, 0.32).
domain_priors:theater_ratio(public_forum_doctrine__traditional_forum_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_forum_doctrine__traditional_forum_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(public_forum_doctrine__traditional_forum_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(public_forum_doctrine__traditional_forum_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_forum_doctrine__traditional_forum_reading, rope).
narrative_ontology:human_readable(public_forum_doctrine__traditional_forum_reading, "Public Forum Doctrine — Traditional Forum Reading").
narrative_ontology:topic_domain(public_forum_doctrine__traditional_forum_reading, "constitutional_law/first_amendment/public_forums").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_forum_doctrine__traditional_forum_reading, '6406320a-b7e9-45ee-b22f-f243fecf21bd').
narrative_ontology:cs_kernel_codification('6406320a-b7e9-45ee-b22f-f243fecf21bd', fixed_text).
narrative_ontology:cs_authority_grounding('6406320a-b7e9-45ee-b22f-f243fecf21bd', lineage).
narrative_ontology:cs_interpretation_layer_present('6406320a-b7e9-45ee-b22f-f243fecf21bd').
narrative_ontology:cs_reading_relation('6406320a-b7e9-45ee-b22f-f243fecf21bd', public_forum_doctrine__designated_forum_reading, coexists_with).
narrative_ontology:cs_reading_relation('6406320a-b7e9-45ee-b22f-f243fecf21bd', public_forum_doctrine__government_speech_reading, coexists_with).
narrative_ontology:cs_axiom('6406320a-b7e9-45ee-b22f-f243fecf21bd', foundational, commons_held_in_trust).
narrative_ontology:cs_axiom_status(commons_held_in_trust, holdable).
narrative_ontology:cs_axiom_grounding('6406320a-b7e9-45ee-b22f-f243fecf21bd', commons_held_in_trust, deontological).
narrative_ontology:cs_axiom('6406320a-b7e9-45ee-b22f-f243fecf21bd', foundational, immemorial_assembly_right).
narrative_ontology:cs_axiom_status(immemorial_assembly_right, holdable).
narrative_ontology:cs_axiom_grounding('6406320a-b7e9-45ee-b22f-f243fecf21bd', immemorial_assembly_right, conventional).
narrative_ontology:cs_reference_frame('6406320a-b7e9-45ee-b22f-f243fecf21bd', immemorial_commons_tradition).
narrative_ontology:cs_drift_state('6406320a-b7e9-45ee-b22f-f243fecf21bd', contemporary_exception_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6406320a-b7e9-45ee-b22f-f243fecf21bd', '').
narrative_ontology:cs_kernel_id(public_forum_doctrine__traditional_forum_reading, public_forum_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_forum_doctrine__traditional_forum_reading, street_level_speakers).
narrative_ontology:constraint_beneficiary(public_forum_doctrine__traditional_forum_reading, protest_movements).
narrative_ontology:constraint_beneficiary(public_forum_doctrine__traditional_forum_reading, assembly_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STREET-LEVEL SPEAKERS (ROPE) — Powerless agents who benefit from the doctrine's coordinate guarantee: public streets and parks are reliably available for assembly, debate, and protest. Low suppression because the doctrine provides genuine protection against closure. The constraint coordinates access; minimal extraction. Exit is mobile — speakers can relocate between forums, but the doctrine anchors their primary options in commons guaranteed accessible.
constraint_indexing:constraint_classification(public_forum_doctrine__traditional_forum_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: MUNICIPAL AUTHORITIES (TANGLED ROPE) — Institutional actors who experience the traditional forum doctrine as a genuine coordination constraint (they must maintain accessible commons for assembly) AND as an extraction mechanism (they lose exclusive control over space, timing, and messaging). The doctrine enforces symmetry: if government uses parks for its own speech (monuments, public ceremonies), it must permit equivalent access to dissenting speakers. Suppression is moderate because municipalities retain significant narrow-tailoring authority (time, place, manner restrictions), but the doctrine prevents complete closure. This perspective sees both coordination function and asymmetric constraint.
constraint_indexing:constraint_classification(public_forum_doctrine__traditional_forum_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROTEST MOVEMENTS (ROPE) — Organized agents whose historical victories (Civil Rights marches, labor demonstrations, women's suffrage organizing) depended on the traditional forum doctrine's guarantee of access. The constraint coordinates collective action: it promises that public streets and parks cannot be unilaterally closed to disfavored speech. These agents experience the doctrine as enabling coordination with minimal coercive overhead. Suppression is low because the doctrine's enforceability has strengthened over 60+ years of case law. Exit options are constrained by scale (movements need citywide/national visibility), but the doctrine removes the threat of sovereign closure.
constraint_indexing:constraint_classification(public_forum_doctrine__traditional_forum_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: COURTS / ANALYTICAL ENFORCER (SCAFFOLD) — The doctrine's enforcement mechanism itself — courts striking down viewpoint-discriminatory denials of park permits and street permits. This perspective sees the constraint as a temporary coordination solution with a structural sunset: as the doctrine is eroded by competing frames (government speech exception, designated forum asymmetry), the enforcement mechanism loses force. Theater is moderate because court review is genuine (not purely performative) but bounded — municipalities can still impose content-neutral time/place/manner restrictions that achieve similar closure effects through technical compliance. The scaffold classification reflects that the doctrine's enforceability depends on continuous judicial re-commitment, not on self-executing legal architecture.
constraint_indexing:constraint_classification(public_forum_doctrine__traditional_forum_reading, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 5: THE DOCTRINE AS INSTITUTIONAL PERFORMANCE (PITON) — From a civilizational view, the traditional forum reading is increasingly theater. Municipalities maintain the rhetorical commitment to the doctrine (viewpoint neutrality, public commons, open streets) while engineering closure through narrow-tailoring requirements, permit fees, police presence, and new designated-forum/government-speech exceptions that carve out large domains from traditional forum protection. The piton perspective observes that the doctrine persists as a canonical reference point in First Amendment discourse, but its functional preservation capacity has degraded. The original function — preventing sovereign erasure of dissident voice — is maintained in form but increasingly eviscerated in practice as alternative doctrinal framings (government speech, designated forums) provide exemptions.
constraint_indexing:constraint_classification(public_forum_doctrine__traditional_forum_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL LAW VIEW — IMMEMORIAL COMMONS (MOUNTAIN) — This perspective grounds the traditional forum reading in deep common law history: public streets and parks have been held in trust for assembly and debate since before state sovereignty, representing an immutable property of democratic governance. The doctrine is seen as codifying a natural law of civic life — that commons cannot be privatized without destroying the conditions for collective speech. However, this mountain classification is a candidate false summit: the doctrine is actually a specific 20th-century judicial construction (Hague v. CIO, 1939), not an immemorial inheritance, and its protections have been systematically eroded by competing doctrinal readings. The 'immemorial' framing naturalizes a historically contingent arrangement.
constraint_indexing:constraint_classification(public_forum_doctrine__traditional_forum_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_forum_doctrine__traditional_forum_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(public_forum_doctrine__traditional_forum_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_forum_doctrine__traditional_forum_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(public_forum_doctrine__traditional_forum_reading, TR),
    TR >= 0.70.

:- end_tests(public_forum_doctrine__traditional_forum_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate. The traditional forum reading does not extract in the classical sense — it coordinates access to public commons. The beneficiaries (street speakers, movements) gain access rights; the apparent 'extraction' from municipal authorities is actually the doctrine's core constraint: municipalities must refrain from viewpoint-discriminatory closure. The extractiveness value reflects the cost to government of maintaining accessible commons rather than the extraction of resources from targets. This is coordination's burden on one party (government) rather than extraction from a victim group. The value is higher than pure rope (0.05–0.35) because the doctrine does constrain government choice, but it remains moderate because the constraint is enabling rather than coercive. Suppression (0.32): Moderate. Municipal authorities have significant tools to restrict assembly within viewpoint-neutrality requirements (time, place, manner restrictions, permit fees, police enforcement). The doctrine prevents complete closure but permits substantial friction. Street-level speakers experience low suppression because the doctrine reliably blocks the most severe restriction (categorical denial); municipalities experience higher suppression because they lose exclusive control. Theater ratio (0.45): Moderate. The doctrine's enforcement mechanism is genuine (courts do overturn viewpoint-discriminatory denials) but increasingly ritualized. Municipalities learn to justify denials in content-neutral language (safety, crowd management, space availability) that technically complies with the doctrine while achieving similar practical closure. The rising trajectory reflects increasing sophistication in how governments engineer compliance-facades that evade doctrine's protection.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary (street speakers see reliable access — rope) and constrained institutional actor (municipalities experience mixed coordination and constraint — tangled rope) is the doctrine's structural signature. Street speakers experience the doctrine as coordination: it removes the threat of sovereign closure and enables planning. Municipalities experience both genuine coordination (they must maintain parks and sidewalks for assembly, which is a collective good) and constraint (they lose discretion to deny access based on viewpoint). The piton perspective observes that as the doctrine is eroded by government-speech and designated-forum exceptions, the coordination function persists in rhetoric while functional capacity degrades. The mountain perspective risks naturalizing the doctrine as immemorial law when it is actually a 20th-century construction.
 *
 * DIRECTIONALITY LOGIC:
 *   Street-level speakers (powerless/mobile/local) derive d from beneficiary status + mobile exit options → low d → negative chi → experience the constraint as enabling rope. Protest movements (organized/constrained/national) derive d from beneficiary status + constrained exit (need city-scale visibility) → moderate d → small positive chi → experience rope with slightly higher perceived constraint cost. Municipal authorities (institutional/constrained/national) derive d from both institutional power AND constrained exit (cannot unilaterally redefine their obligations) + net victim status (they lose exclusive control) → d around 0.55–0.65 → higher chi → experience tangled rope. The piton perspective's high institutional power and arbitrage exit would normally produce low chi, but the piton classification derives from theater_ratio (0.45) rather than from directionality — the doctrine is degraded by exception-creep, not by extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not require mandatrophy resolution because its base extractiveness (0.28) does not exceed 0.70. However, the perspectival structure reveals a latent mandatrophy: the traditional forum reading claims to coordinate public assembly (rope function) while simultaneously constraining government discretion (extraction mechanism). The constraint is genuinely both at once — it enables street speakers while restricting municipal authorities. The doctrine resolves this apparent mandatrophy by classifying the constraint as a coordination mechanism WITH asymmetric burdens rather than as pure extraction. The burden (maintaining accessible commons) falls on government; the benefit (reliable access) accrues to speakers. Both perspectives are structurally correct because the doctrine's function IS asymmetric distribution of a public good.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immemorial_vs_constructed_status,
    'Is the traditional forum doctrine rooted in genuinely immemorial common law practice, or is it a 20th-century doctrinal construction that retroactively claims ancient lineage?',
    'Genealogical analysis: (1) Pre-1939 case law and practice patterns regarding street/park assembly in England and America; (2) comparison of historical permit denial rates and closure mechanisms before and after Hague v. CIO (1939); (3) scholarly historiography of common law assembly rights vs state police power',
    'If immemorial: the mountain perspective is justified — the doctrine describes an inherent property of democratic commons. If constructed: the mountain is a false summit — the doctrine naturalizes a contingent institutional choice, making it harder to reform. This omega directly determines whether the engine''s false_summit_mountain signature should fire.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immemorial_vs_constructed_status, empirical, 'Whether the traditional forum doctrine reflects immemorial common law or 20th-century construction').

omega_variable(
    government_speech_exception_foreclosure,
    'Does the government speech exception (monuments, license plates, public programs) logically foreclose the traditional forum reading, or do both readings coexist in contemporary doctrine?',
    'Doctrinal analysis: (1) Can a government speaker (monarch, city council) simultaneously hold space in trust for commons assembly AND claim exclusive speech authority over that space? (2) Supreme Court opinions examining whether government speech erodes traditional forum status (case study: Sons of Confederate Veterans v. Daughters of Union Veterans — Confederate monuments in public parks); (3) whether lower courts have maintained traditional forum protections despite government speech exception',
    'If foreclosure: the traditional and government-speech readings cannot coexist in a single constitutional framework — the reading_relations should be forecloses. If coexistence: they inhabit different doctrinal spaces (government speech applies to government-created content; traditional forum applies to third-party speakers). This determines the relation type in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(government_speech_exception_foreclosure, conceptual, 'Whether government speech exception logically forecloses traditional forum reading').

omega_variable(
    designated_forum_creep_mechanism,
    'Does the designated forum reading (government opens a forum, then viewpoint discrimination becomes permissible once the forum is ''closed'') structurally erode or coexist with the traditional forum reading?',
    'Empirical analysis of permit denial patterns: (1) Do municipalities increasingly classify traditional public spaces (downtown plazas, city parks) as ''designated'' rather than ''traditional'' to enable viewpoint discrimination? (2) Measure the ratio of permits denied under designated-forum vs traditional-forum doctrine before/after designated forum doctrine emerged (approximately 1983 with Perry Education Association v. Perry Educators Association); (3) qualitative review of permit denial justifications to identify doctrinal frame-switching',
    'If creep is systematic: the designated forum reading influences (creates structural pressure on) the traditional forum reading by progressively narrowing its domain. If courts maintain clear categorical boundaries: the readings coexist without significant interference. This determines whether relations should be influences or coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(designated_forum_creep_mechanism, empirical, 'Whether designated forum doctrine systematically erodes traditional forum protections').

omega_variable(
    enforceability_decay_trajectory,
    'Is the traditional forum doctrine''s enforceability measurably declining, or is the piton perspective''s claim of degraded function merely retrospective?',
    'Longitudinal empirical analysis: (1) Track successful permit challenges (appellate court reversals of permit denials) per decade since 1939 to measure enforcement success rate; (2) measure permit denial justifications (objective vs pretextual time/place/manner restrictions); (3) analyze police enforcement intensity (arrests at unpermitted or marginally-compliant assemblies) as proxy for municipal willingness to test enforcement limits; (4) survey municipal legal opinions on permit denial scope — do attorneys treat traditional forum as a binding constraint or as a negotiable boundary?',
    'If enforceability is declining: the piton classification is structurally justified — the doctrine persists in form but its functional capacity to prevent closure is degrading. If enforceability is stable: the piton is misclassifying a genuinely functional rope as merely performative. This directly tests the theater_ratio value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforceability_decay_trajectory, empirical, 'Enforceability trend for traditional forum doctrine protections').

omega_variable(
    kernel_authority_grounding_ambiguity,
    'Does the traditional forum doctrine ground its authority in lineage (common law continuity), expertise (judicial wisdom about democratic requirements), or extraction (institutional interests in controlling public space)?',
    'Doctrinal genealogy: (1) Analyze Supreme Court opinions grounding the doctrine — what legitimacy claims do courts make (ancient tradition, natural law, constitutional text, functional democracy requirement)? (2) Compare the doctrine''s treatment in originalist vs living-constitutionalist jurisprudence — does the authority claim depend on temporal location (immemorial vs modern)? (3) Examine state-level variation — do states with strong common law traditions enforce traditional forum doctrine differently than code-law or newer states?',
    'If grounded in lineage: authority is vulnerable to the constructed-vs-immemorial omega. If grounded in expertise (functional democracy): authority is more robust but requires continuous judicial enforcement. If grounded in extraction (state interests): the doctrine''s victim set expands. This determines the authority_grounding value in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_authority_grounding_ambiguity, conceptual, 'Authority grounding for traditional forum doctrine — lineage, expertise, or extraction-based').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_forum_doctrine__traditional_forum_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_forum_doctrine__traditional_forum_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(publ_tr_t20, public_forum_doctrine__traditional_forum_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(publ_tr_t40, public_forum_doctrine__traditional_forum_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_forum_doctrine__traditional_forum_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(publ_be_t20, public_forum_doctrine__traditional_forum_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(publ_be_t40, public_forum_doctrine__traditional_forum_reading, base_extractiveness, 40, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_forum_doctrine__traditional_forum_reading, identity_coordination).
narrative_ontology:affects_constraint(public_forum_doctrine__traditional_forum_reading, public_forum_doctrine__designated_forum_reading).
narrative_ontology:affects_constraint(public_forum_doctrine__traditional_forum_reading, public_forum_doctrine__government_speech_reading).

% DUAL FORMULATION NOTE:
% The public forum doctrine kernel has three structurally distinct constraint readings. Each represents a different answer to what legal authority grounds access to public space: immemorial commons (traditional), government prerogative (designated), or government speech rights (government speech). The three readings are linked in network as coexisting siblings within one kernel. The traditional forum reading (this story) emphasizes immemorial commons and strongest protection; the designated forum reading emphasizes government choice; the government speech reading emphasizes government speaker status. Each reading produces different victim sets and beneficiary groups. All three appear simultaneously in contemporary Supreme Court doctrine applied to overlapping fact patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
