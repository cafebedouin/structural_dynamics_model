% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__corporate_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__corporate_enclosure_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__corporate_enclosure_reading
 *   human_readable: Copyright as Maximalist Property Right (Corporate Enclosure Reading)
 *   domain: intellectual_property/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the corporate enclosure reading of the
 *   copyright constitutional mandate kernel. The reading interprets 'limited
 *   times' (U.S. Constitution Article I, Section 8) as permitting maximal
 *   copyright extension short of explicit perpetuity, and treats copyright as
 *   a property right requiring maximal protection against fair-use
 *   carve-outs, circumvention, and derivative works. This reading has been
 *   the dominant statutory interpretation since the Sonny Bono Copyright Term
 *   Extension Act (1998) and was affirmed in Eldred v. Ashcroft (2003). Under
 *   this reading, copyright is a tangled rope—genuinely coordinating
 *   incumbent media access and distribution (the real coordination problem),
 *   but doing so through asymmetric extraction from derivative creators,
 *   educators, and archivists (the real victims). The constraint requires
 *   active enforcement: DMCA anticircumvention law, Content ID automated
 *   takedowns, cease-and-desist campaigns, and lobbied term extension. The
 *   measured extraction (0.81) is substantially higher than the constraint's
 *   coordination narrative would suggest, signaling that the arrangement
 *   persists not because of the founding incentive problem (which empirical
 *   evidence shows is structurally solved) but because it enables monopoly
 *   rent collection. Sibling readings (public_scaffold_reading and
 *   judicial_ambiguity_reading) contest this interpretation, treating
 *   copyright as a temporary monopoly in service of public domain, or as a
 *   legislative discretion zone where Congress's choices require rational
 *   basis review but not automatic maximalism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.81).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.76).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Copyright as Maximalist Property Right (Corporate Enclosure Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "intellectual_property/constitutional_law").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, 'c8b8979e-29f5-40b6-9a5e-663fe00a9683').
narrative_ontology:cs_kernel_codification('c8b8979e-29f5-40b6-9a5e-663fe00a9683', fixed_text).
narrative_ontology:cs_authority_grounding('c8b8979e-29f5-40b6-9a5e-663fe00a9683', extraction).
narrative_ontology:cs_interpretation_layer_present('c8b8979e-29f5-40b6-9a5e-663fe00a9683').
narrative_ontology:cs_reading_relation('c8b8979e-29f5-40b6-9a5e-663fe00a9683', copyright_constitutional_mandate__public_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8b8979e-29f5-40b6-9a5e-663fe00a9683', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('c8b8979e-29f5-40b6-9a5e-663fe00a9683', foundational, copyright_as_maximalist_property_right).
narrative_ontology:cs_axiom_status(copyright_as_maximalist_property_right, holdable).
narrative_ontology:cs_axiom_grounding('c8b8979e-29f5-40b6-9a5e-663fe00a9683', copyright_as_maximalist_property_right, deontological).
narrative_ontology:cs_axiom('c8b8979e-29f5-40b6-9a5e-663fe00a9683', foundational, limited_times_permits_maximal_extension).
narrative_ontology:cs_axiom_status(limited_times_permits_maximal_extension, holdable).
narrative_ontology:cs_axiom_grounding('c8b8979e-29f5-40b6-9a5e-663fe00a9683', limited_times_permits_maximal_extension, instrumental).
narrative_ontology:cs_axiom('c8b8979e-29f5-40b6-9a5e-663fe00a9683', secondary, incentive_justifies_extraction).
narrative_ontology:cs_axiom_status(incentive_justifies_extraction, overridden).
narrative_ontology:cs_axiom_grounding('c8b8979e-29f5-40b6-9a5e-663fe00a9683', incentive_justifies_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('c8b8979e-29f5-40b6-9a5e-663fe00a9683', copyright_as_property_protection_mandate).
narrative_ontology:cs_drift_state('c8b8979e-29f5-40b6-9a5e-663fe00a9683', contemporary_post_sonny_bono_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c8b8979e-29f5-40b6-9a5e-663fe00a9683', '2026-06-13T14:32:15Z').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_media_incumbents).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, major_recording_industry).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, film_studios).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, archivists).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, remix_artists).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Disney, major studios, and consolidated publishing houses set the copyright agenda through legislative lobbying, trade negotiation, and strategic litigation. They benefit directly from copyright term extension (Mickey Mouse protection, catalog monopoly control, derivative work blocking). They define what 'limited times' means through statutory interpretation and enforce it through DMCA litigation and cease-and-desist campaigns. Their exit option is arbitrage: they can shift production between jurisdictions with favorable copyright regimes or lobby for harmonized international protection.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_media_incumbents, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_media_incumbents, beneficiary).

% RIAA, IFPI, and major record labels capture the entire pre-1972 sound recording royalty stream through copyright monopoly. They have successfully lobbied for term extension (Sonny Bono Act, EU directives) and criminalized circumvention of Digital Rights Management. They collect enforcement rents from ISP takedown compliance. They depend on copyright maximalism to prevent open licensing of catalog.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, major_recording_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Major film studios (Warner, Paramount, Universal) benefit from copyright maximalism through theatrical release windows, streaming exclusivity, and blocking of fan works. They lobby for term extension and use DMCA enforcement to prevent unauthorized streaming, repair of DRM-locked media, and derivative adaptations. Their business model depends on copyright monopoly preventing market segmentation via open access.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, film_studios, beneficiary,
    institutional, generational, arbitrage, global).

% Songwriters sampling existing compositions, authors writing sequels or retellings, filmmakers creating fan works, musicians remixing recordings—all must navigate copyright thickets. They pay through cease-and-desist letters, litigation threats, YouTube takedowns, and self-censorship. Their exit option is identity-locked: operating outside copyright means abandoning distribution channels and audience reach controlled by incumbents. Aspiring creators internalize the rule that 'you cannot use without permission,' making professional identity and copyright compliance fused.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators, payer,
    moderate, biographical, identity_locked, global).

% Teachers and librarians operate under narrow fair-use carve-outs that force them to navigate licensing fees, licensing denial, and perpetual uncertainty. Copyright term extension pushes educational materials into commercial pricing regimes just as they would have entered public domain. They pay through licensing fees, materials unavailability, and curricular constraint. Their exit option is constrained by institutional policy and accreditation requirements requiring copyrighted materials.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, educators, payer,
    organized, biographical, constrained, national).

% Digital preservation specialists face copyright-based injunctions against format migration (to keep digital materials readable as technology changes), circumventing access controls to preserve at-risk materials, and creating backup copies. The Library of Congress can make narrow exemptions, but their structural position is trapped: copyright maximalism threatens the ability to preserve cultural heritage. They pay through preservation delays, material loss, and restricted access even to archived works.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, archivists, payer,
    moderate, biographical, trapped, national).

% Electronic musicians, video remixers, collage artists whose practice DEPENDS on recombination of existing cultural material. Copyright maximalism treats their artistic identity as infringement. They are identity-locked: their art form is defined as copyright violation under the constraint. Their exit is theoretically 'stop creating remix art,' which is equivalent to ceasing to be an artist. They operate in legal gray zones, on platforms (YouTube) that automate takedown, paying through demonetization and suppression.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, remix_artists, payer,
    powerless, biographical, identity_locked, global).

% Scholars studying cultural history, lawyers doing comparative copyright analysis, technologists building preservation tools—all face copyright barriers to public materials that should be open. Term extension keeps materials under copyright monopoly longer. They pay through research delays, unavailable sources, and restricted datasets. Their exit option is constrained: they cannot exit the copyright system without abandoning evidence-based work.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_researchers, payer,
    moderate, biographical, constrained, national).

% Congress holds constitutional power to set copyright terms and conditions. The corporate enclosure reading privileges their delegated power to maximize terms as constitutional interpretation, treating 'limited times' as a rhetorical limit rather than a strict temporal bound. Congress receives lobbying resources and campaign support from beneficiaries. Their exit is analytical: they could reframe the constitutional mandate, but this reading closes that option by treating maximalism as the constitutional default.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, congress, agenda_setter,
    institutional, generational, analytical, national).

% Federal courts have declined to question term extension on constitutional grounds (Eldred v. Ashcroft), treating rational-basis review as the standard and deferring to Congress. Courts enforce DMCA against circumvention regardless of whether circumvention enables fair-use activity. Courts operate under the corporate enclosure reading as the operative constitutional interpretation, enabling the other agents' actions.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% Creative Commons, open-culture organizations, and remix-culture advocates would argue for shorter terms, broader fair-use, and technical freedom to circumvent access controls. They are excluded from the legislative consensus that treats copyright maximalism as constitutional mandate. They operate through alternative licensing (Creative Commons), open-access publishing, and Internet Archive—structurally outside the constraint rather than within it.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, open_licensing_advocates, excluded,
    powerful, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_media_incumbents).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__corporate_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Copyright term extension coordinates incumbent media companies in preventing market segmentation: once a work enters public domain, independent creators can undercut incumbents' pricing and licensing fees. By extending copyright to the maximum permissible length ('limited times' interpreted as near-perpetuity), incumbents prevent low-cost derivative works from competing against their own commercial editions. The coordination problem solved is rent capture and market foreclosure.
% TRANSFER_FUNCTION: Moves licensing revenue from derivative creators, educators, and institutions to corporate copyright holders through three mechanisms: (1) direct licensing fees charged for permission to quote, adapt, or remix; (2) takedown-driven demonetization (YouTube Content ID, Spotify DMCA enforcement); (3) term extension that keeps catalog works under monopoly control when they would otherwise fall into public domain and enable competitive derivatives.
% ABSENT_VOICES: Artists and creators who would advocate for shorter terms and broader fair-use rights are systematically excluded from legislative copyright policy-making. The legislative consensus treats copyright maximalism as constitutional mandate, closing the debate. Independent scholars, remix artists, and open-culture advocates operate outside the policy process entirely—their testimony appears in amicus briefs and academic papers, not in the legislative record that treats term extension as inevitable.
% DISAPPEARANCE_RATIONALE: If copyright maximalism constraint disappeared overnight, within months: (1) tens of thousands of 20th-century works would enter public domain; (2) derivative industries (remix, remix music production, open educational resources) would explode; (3) pricing for educational and archival materials would collapse; (4) corporate incumbents' business models built on catalog monopoly would fragment; (5) open-access publishing and preservation would accelerate. The constraint's persistence depends on active enforcement and legislative extension, not on participant consent.
% FOUNDING_PROBLEM: Copyright was established to incentivize creation by granting temporary monopoly to authors and publishers. The founding constitutional problem: how to balance creator incentive against public access to knowledge and culture.
% FOUNDING_PROBLEM_CORROBORATION: Empirical evidence from outside beneficiary circles: (1) Copyright Office studies showing 99% of 20th-century works are not in commercial circulation; (2) Economic analyses showing copyright incentive is not the primary driver of content production (employees create for salaries, not copyright monopoly); (3) International case studies showing shorter terms and broader fair-use do not reduce creation; (4) Congressional Research Service acknowledges the incentive rationale does not justify near-perpetual terms. The founding problem (incentivizing creation) is structurally solved; the constraint persists to extract monopoly rents, not to solve the original problem.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__corporate_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__corporate_enclosure_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__corporate_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) and rising over the 1976–2026 interval (0.62 → 0.81) because the constraint's function shifted from incentivizing creation (1976: a genuine coordination problem) to preventing public-domain enclosure and derivative work competition (2026: pure monopoly rent). The term extension pattern shows this drift: 1976 had a 56-year term (life + 50); 2026 has a 70-year term (life + 70), with no corresponding increase in creation—the measured base_extractiveness rise captures rent-seeking layered onto the original coordination. Suppression is high (0.76) and rising (0.55 → 0.76) because enforcement machinery intensified: the DMCA (1998) criminalized circumvention regardless of fair-use intent; Content ID automated takedowns at scale; cease-and-desist became routinized. The structural-level suppression (0.82 at 2026) exceeds individual-level suppression (0.72) because the constraint operates through institutional policy (YouTube, Spotify, ISPs automating compliance) not just individual legal threat. Theater_ratio is moderate-low (0.42) and stable because copyright enforcement is functionally about blocking derivative works and maintaining monopoly control, not actually about incentivizing original creation—the security/incentive narrative is the cover story, but enough real enforcement activity does exist that theater doesn't dominate. Accessibility collapse is high (0.68 at 2026) and rising because open-access alternatives remain constrained by copyright enforcement; remix, derivative, and open-culture practices exist but at reduced scale and with internalized legal risk. Resistance is moderate (0.71) and declining slightly (0.72 → 0.65 structurally) because organized resistance (Creative Commons, open-source, open-access movements) exists but has been contained by incumbent institutional power and has not dislodged copyright maximalism from legislative consensus.
 *
 * PERSPECTIVAL GAP:
 *   From the corporate agenda-setter seat, the constraint is tangled rope: a genuine coordination function (ensuring efficient distribution, licensing, and quality control in media markets) with acceptable collateral costs (fair-use restrictions, derivative blocking) to maintain the coordination. From the derivative-creator seat, the same arrangement is a snare: the coordination function is pretextual, and the constraint's actual purpose is blocking low-cost derivative competition and enforcing monopoly pricing on cultural access. The payer and beneficiary seats compute wildly different types because the structural asymmetry is extreme: asymmetry in power (institutional vs. moderate/powerless), exit options (arbitrage vs. trapped/identity-locked), and the directionality derivation amplifies the extraction gap. The engine should compute tangled_rope from the beneficiary-side structural narrative (real coordination + active enforcement) and snare from the payer-side structural narrative (coordination narrative is cover; extraction is the actual function). This divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Corporate media incumbents (Disney, RIAA, MPAA) sit at d ≈ 0.0 (full beneficiary): they set agenda, collect licensing fees, block derivatives, and have arbitrage exit (shift production, harmonize internationally). Derivative creators sit at d ≈ 1.0 (full target): they are identity-locked (their artistic practice is defined by copyright violation), constrained in exit (can't distribute without copyright clearance), and directly extract from through cease-and-desist and demonetization. Educators sit at d ≈ 0.85: they are organizational payers (licensing fees, material unavailability), constrained exit (institutional policy), and absorb term-extension costs without collecting benefits. Archivists sit at d ≈ 0.95: they are trapped (can't preserve without circumventing access controls; circumvention is criminalized). Congress and courts sit at d ≈ 0.5 (symmetric/analytical): they maintain the arrangement through deference and rational-basis review, neither collecting from it nor bearing direct costs, but structurally maintaining beneficiary positions by foreclosing alternative readings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (incentivizing creation) is structurally dead but the constraint persists and has intensified. Copyright's original function was to solve a coordination problem: how to incentivize authors and publishers to invest in creation by granting temporary monopoly. By 1976, economic evidence showed that copyright incentive is not the primary driver of content creation (employees create for salaries, institutional IP drives academic/scientific creation, derivative works drive commercial innovation). The founding problem is solved by alternative mechanisms. The constraint persists because it now extracts monopoly rents independent of creation incentive. Term extension (Sonny Bono 1998, EU directives) shows the function shift: extending terms for already-created works provides zero creation incentive but captures 20+ additional years of monopoly rent. The measured founding_problem_status = dead and disappearance_verdict = world_rearranges together flag a zombie constraint: one that no longer solves its founding problem but persists because beneficiaries capture the extraction and have blocked alternatives. The theater_ratio (0.42) reflects this: some enforcement activity still nominally defends creation incentive, but the majority of enforcement activity (term extension lobbying, DMCA anticircumvention, Content ID automation) is pure rent defense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incentive_vs_monopoly_rent,
    'Is the measured extractiveness serving genuine creation incentive or primarily enabling monopoly rent extraction from already-created works?',
    'Empirical comparison: measure creation rates and quality across jurisdictions with different copyright terms (shorter terms in some regions, longer in others) and analyze term extension effects on new creation. Copyright Office study of commercially available 20th-century works vs. copyright-protected inventory.',
    'If creation incentive is the actual function, extractiveness should drop when measuring only active-creation scenarios (current authors incentivizing new works) and rise when measuring catalog monopoly. If the majority of measured extractiveness is monopoly rent on already-created works, the constraint should be reclassified from tangled_rope (coordination + extraction) toward snare (extraction as primary function with coordination as narrative cover). Current measurement assumes both are present; resolving the proportion would refine the classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incentive_vs_monopoly_rent, empirical, 'Whether the constraint''s extractiveness serves genuine creation incentive or monopoly rent.').

omega_variable(
    constitutional_interpretation_contest,
    'Is the corporate enclosure reading''s interpretation of ''limited times'' as maximal extension constitutionally justified, or is the constraint a false constitutional summit (beneficiaries claiming constitutional mandate for a policy choice)?',
    'Judicial reinterpretation of Eldred standard: courts could treat ''limited times'' as a substantive constraint (not merely a rational-basis trigger), requiring Congress to justify term length by reference to remaining creation-incentive needs rather than treating maximalism as the constitutional default. Alternatively, a constitutional amendment clarifying intent.',
    'If the constraint is a false summit, FSM triggers and reclassification path redirects toward snare (the beneficiaries'' narrative of constitutional mandate is revealed as constructed). If the constraint survives judicial reinterpretation as genuinely constitutional, the corporate enclosure reading holds as the authoritative reading and extraction persists with heightened legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_interpretation_contest, conceptual, 'Whether ''limited times'' is a substantive constitutional constraint or a legislative discretion zone the corporate enclosure reading treats as maximalism-permissive.').

omega_variable(
    internalized_suppression_vs_structural,
    'What proportion of the measured suppression (0.76) is structural (legal enforcement through courts, ISPs, platforms) versus internalized (creators have absorbed the rule that copyright violation is inevitable and self-censor without external threat)?',
    'Post-exit tracking: in jurisdictions that legalize circumvention or shorten copyright terms, measure whether suppression drops structurally or whether creators continue to self-censor due to internalized compliance norms. Empirical study of remix/derivative communities in open-licensing contexts (Creative Commons) versus copyright-maximalist contexts.',
    'If suppression is primarily structural (enforcement-driven), removing the constraint removes suppression quickly. If suppression is primarily internalized, removing the constraint leaves residual suppression; creators need re-education and community rebuilding to recognize that the constraint has changed. High internalized suppression indicates identity-locking is deeper than the measured exit_options capture, requiring narrative reframing beyond legal change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_vs_structural, empirical, 'Proportion of copyright suppression that is structural enforcement versus internalized self-censorship.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the corporate enclosure reading''s interpretation of ''limited times'' as maximalism-permissive logically foreclose the public_scaffold_reading''s interpretation of copyright as temporary monopoly in service of public domain?',
    'Structural analysis of the two readings'' axioms: do they contradict at the foundational level (foreclosure), or do they occupy different institutional seats and political coalitions (coexistence)? Can a single legal framework hold both readings simultaneously without internal contradiction?',
    'If foreclosure: the corporate enclosure reading is the unique constitutional reading and the public_scaffold_reading is analytically untenable within U.S. constitutional law. If coexistence: both readings are live positions; the question is political power (which reading dominates) rather than logical consistency. Current assessment: coexists_with (they are held by different coalitions and represent different policy choices, not contradictory logical necessities).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether the corporate enclosure reading logically rules out the public_scaffold_reading or whether both remain live within a single constitutional framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 1976, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1976, 0.2).
narrative_ontology:measurement(copy_tr_t1990, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1998, 0.35).
narrative_ontology:measurement(copy_tr_t2005, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(copy_tr_t2015, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(copy_tr_t2026, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1976, 0.62).
narrative_ontology:measurement(copy_be_t1990, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1998, 0.76).
narrative_ontology:measurement(copy_be_t2005, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2005, 0.79).
narrative_ontology:measurement(copy_be_t2015, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2015, 0.8).
narrative_ontology:measurement(copy_be_t2026, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2026, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1976, 0.55).
narrative_ontology:measurement(copy_su_t1990, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1990, 0.64).
narrative_ontology:measurement(copy_su_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1998, 0.71).
narrative_ontology:measurement(copy_su_t2005, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2005, 0.74).
narrative_ontology:measurement(copy_su_t2015, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(copy_su_t2026, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2026, 0.76).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1976, tn=2026
narrative_ontology:measurement(copy_grid_01, copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse(class), 1976, 0.52).
narrative_ontology:measurement(copy_grid_02, copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse(class), 2026, 0.7).
narrative_ontology:measurement(copy_grid_03, copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse(individual), 1976, 0.45).
narrative_ontology:measurement(copy_grid_04, copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse(individual), 2026, 0.62).
narrative_ontology:measurement(copy_grid_05, copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse(organizational), 1976, 0.55).
narrative_ontology:measurement(copy_grid_06, copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse(organizational), 2026, 0.75).
narrative_ontology:measurement(copy_grid_07, copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse(structural), 1976, 0.58).
narrative_ontology:measurement(copy_grid_08, copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse(structural), 2026, 0.78).
narrative_ontology:measurement(copy_grid_09, copyright_constitutional_mandate__corporate_enclosure_reading, resistance(class), 1976, 0.65).
narrative_ontology:measurement(copy_grid_10, copyright_constitutional_mandate__corporate_enclosure_reading, resistance(class), 2026, 0.58).
narrative_ontology:measurement(copy_grid_11, copyright_constitutional_mandate__corporate_enclosure_reading, resistance(individual), 1976, 0.6).
narrative_ontology:measurement(copy_grid_12, copyright_constitutional_mandate__corporate_enclosure_reading, resistance(individual), 2026, 0.55).
narrative_ontology:measurement(copy_grid_13, copyright_constitutional_mandate__corporate_enclosure_reading, resistance(organizational), 1976, 0.68).
narrative_ontology:measurement(copy_grid_14, copyright_constitutional_mandate__corporate_enclosure_reading, resistance(organizational), 2026, 0.62).
narrative_ontology:measurement(copy_grid_15, copyright_constitutional_mandate__corporate_enclosure_reading, resistance(structural), 1976, 0.72).
narrative_ontology:measurement(copy_grid_16, copyright_constitutional_mandate__corporate_enclosure_reading, resistance(structural), 2026, 0.65).
narrative_ontology:measurement(copy_grid_17, copyright_constitutional_mandate__corporate_enclosure_reading, stakes_inflation(class), 1976, 0.38).
narrative_ontology:measurement(copy_grid_18, copyright_constitutional_mandate__corporate_enclosure_reading, stakes_inflation(class), 2026, 0.64).
narrative_ontology:measurement(copy_grid_19, copyright_constitutional_mandate__corporate_enclosure_reading, stakes_inflation(individual), 1976, 0.35).
narrative_ontology:measurement(copy_grid_20, copyright_constitutional_mandate__corporate_enclosure_reading, stakes_inflation(individual), 2026, 0.58).
narrative_ontology:measurement(copy_grid_21, copyright_constitutional_mandate__corporate_enclosure_reading, stakes_inflation(organizational), 1976, 0.42).
narrative_ontology:measurement(copy_grid_22, copyright_constitutional_mandate__corporate_enclosure_reading, stakes_inflation(organizational), 2026, 0.68).
narrative_ontology:measurement(copy_grid_23, copyright_constitutional_mandate__corporate_enclosure_reading, stakes_inflation(structural), 1976, 0.4).
narrative_ontology:measurement(copy_grid_24, copyright_constitutional_mandate__corporate_enclosure_reading, stakes_inflation(structural), 2026, 0.7).
narrative_ontology:measurement(copy_grid_25, copyright_constitutional_mandate__corporate_enclosure_reading, suppression(class), 1976, 0.52).
narrative_ontology:measurement(copy_grid_26, copyright_constitutional_mandate__corporate_enclosure_reading, suppression(class), 2026, 0.78).
narrative_ontology:measurement(copy_grid_27, copyright_constitutional_mandate__corporate_enclosure_reading, suppression(individual), 1976, 0.48).
narrative_ontology:measurement(copy_grid_28, copyright_constitutional_mandate__corporate_enclosure_reading, suppression(individual), 2026, 0.72).
narrative_ontology:measurement(copy_grid_29, copyright_constitutional_mandate__corporate_enclosure_reading, suppression(organizational), 1976, 0.55).
narrative_ontology:measurement(copy_grid_30, copyright_constitutional_mandate__corporate_enclosure_reading, suppression(organizational), 2026, 0.8).
narrative_ontology:measurement(copy_grid_31, copyright_constitutional_mandate__corporate_enclosure_reading, suppression(structural), 1976, 0.58).
narrative_ontology:measurement(copy_grid_32, copyright_constitutional_mandate__corporate_enclosure_reading, suppression(structural), 2026, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__corporate_enclosure_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__corporate_enclosure_reading, 0.22).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, dmca_anticircumvention_criminal_liability).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, fair_use_jurisprudential_narrowing).

% DUAL FORMULATION NOTE:
% The copyright_constitutional_mandate kernel decomposes into three constraint stories: corporate_enclosure_reading (this file: copyright as maximalist property right, high extraction, beneficiaries=incumbents), public_scaffold_reading (copyright as temporary monopoly serving public domain, lower extraction, beneficiaries=public researchers), and judicial_ambiguity_reading (term length as legislative discretion, rational-basis deference). Each reading instantiates a different constraint with different ε, beneficiary/victim structure, and type. The sibling readings are linked via network.affects_constraints. Each reading's axioms and reference_frame are distinct; the kernel is the shared 'limited times' constitutional text that all three readings interpret differently. The corporate_enclosure_reading dominates current U.S. statutory policy (Sonny Bono 1998, DMCA 1998, Eldred 2003); the public_scaffold_reading is the minority position held by Creative Commons advocates and copyright reformers; the judicial_ambiguity_reading appears as Eldred's formal rational-basis posture but does not endorse any particular term-length policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_constitutional_mandate__corporate_enclosure_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
